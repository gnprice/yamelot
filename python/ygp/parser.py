from __future__ import absolute_import
from __future__ import print_function

from collections import OrderedDict, Mapping


class YGPError(Exception):
    pass


class YGPValueError(YGPError, ValueError):
    pass


class YAMLEventHandler(object):

    def __init__(self, clib, parser):
        self.clib = clib
        self.parser = parser
        self.stack = []
        self.anchors = {}

        self.cur_obj = None
        self.cur_in = False
        self.map_key = None

        self.last_obj = None

    def build_libyaml_error(self):
        buf = []
        if self.parser.problem != self.clib.NULL:
            buf.append(self.clib.string(self.parser.problem))

            buf.append(
                'at line: {} col: {}'.format(
                    self.parser.problem_mark.line + 1,
                    self.parser.problem_mark.column + 1,
                )
            )

        if self.parser.context != self.clib.NULL:
            buf.append(self.clib.string(self.parser.context))

            buf.append(
                'at line: {} col: {}'.format(
                    self.parser.context_mark.line + 1,
                    self.parser.context_mark.column + 1,
                )
            )

        return YGPValueError(' '.join(buf))

    def build_custom_error(self, msg):
        buf = [
            msg,
            'at line: {} col: {}'.format(
                self.parser.mark.line + 1,
                self.parser.mark.column + 1,
            )
        ]
        return YGPValueError(' '.join(buf))

    def push_state(self):
        self.stack.append((
            self.cur_obj,
            self.cur_in,
            self.map_key,
        ))

    def pop_state(self):
        self.last_obj = self.cur_obj
        (
            self.cur_obj,
            self.cur_in,
            self.map_key,
        ) = self.stack.pop()

    def set_map(self, value):
        assert self.cur_in == 'map'
        assert self.map_key
        if self.map_key == '<<':
            defaults = value
            if isinstance(defaults, Mapping):
                defaults = [defaults]
            for default in defaults:
                for key, value in default.items():
                    if key not in self.cur_obj:
                        self.cur_obj[key] = value
        else:
            self.cur_obj[self.map_key] = value
        self.map_key = None

    def convert_scalar(self, value):
        if value.isdigit():
            if value[0] == '0':
                raise self.build_custom_error(
                    'Octal scalers are not supported {!r}'.format(value)
                )
            value = int(value)
        elif value == 'null':
            value = None
        elif value == 'true':
            value = True
        elif value == 'false':
            value = False
        return value

    def add_anchor(self, anchor, value):
        if anchor != self.clib.NULL:
            anchor_value = self.clib.string(anchor)
            if anchor_value in self.anchors:
                raise self.build_custom_error(
                    'Anchor {} already in use'.format(anchor_value)
                )
            self.anchors[anchor_value] = value

    def check_tag(self, tag):
        if tag != self.clib.NULL:
            raise self.build_custom_error(
                'Tags are not allowed tag={!r}'.format(self.clib.string(tag))
            )

    def add_item(self, value):
        if self.cur_in == 'seq':
            self.cur_obj.append(value)
        elif self.cur_in == 'map' and self.map_key is None:
            self.map_key = value
        elif self.cur_in == 'map' and self.map_key:
            self.set_map(value)
        else:
            # NOOP root object
            pass

    def process(self):
        event = self.clib.new_event()

        while True:
            if not self.clib.parser_parse(self.parser, event):
                self.clib.event_delete(event)
                raise self.build_libyaml_error()

            type_ = event.type

            if type_ == self.clib.SCALAR_EVENT:

                scalar = event.data.scalar

                value = self.clib.string(
                    scalar.value,
                    scalar.length,
                )

                if scalar.style == self.clib.FOLDED_SCALAR_STYLE:
                    raise self.build_custom_error(
                        'Folded Scalars are not allowed {!r}'.format(
                            value
                        )
                    )

                if scalar.style == self.clib.PLAIN_SCALAR_STYLE:
                    if value == '':
                        raise self.build_custom_error('Missing value')
                    if value == '~':
                        raise self.build_custom_error(
                            '\'~\' not allowed as an alias to null'
                        )

                if scalar.style == self.clib.LITERAL_SCALAR_STYLE:
                    if self.cur_in == 'map' and self.map_key is None:
                        raise self.build_custom_error(
                            'Literal scalers are not allowed as keys'
                        )

                self.check_tag(scalar.tag)

                if scalar.style == self.clib.PLAIN_SCALAR_STYLE:
                    value = self.convert_scalar(value)

                self.add_anchor(scalar.anchor, value)
                self.add_item(value)

            elif type_ == self.clib.ALIAS_EVENT:
                anchor = self.clib.string(event.data.alias.anchor)
                try:
                    anchor_value = self.anchors[anchor]
                except KeyError:
                    raise self.build_custom_error(
                        'Anchor %s not found' % (
                            self.clib.string(event.data.alias.anchor),
                        )
                    )
                self.add_item(anchor_value)

            elif type_ == self.clib.SEQUENCE_START_EVENT:
                new_seq = []
                self.add_anchor(event.data.sequence_start.anchor, new_seq)
                self.check_tag(event.data.sequence_start.tag)
                self.add_item(new_seq)

                self.push_state()
                self.cur_obj = new_seq
                self.cur_in = 'seq'

            elif type_ == self.clib.SEQUENCE_END_EVENT:
                self.pop_state()

            elif type_ == self.clib.MAPPING_START_EVENT:
                new_map = OrderedDict()
                self.add_anchor(event.data.mapping_start.anchor, new_map)
                self.check_tag(event.data.mapping_start.tag)
                self.add_item(new_map)

                self.push_state()
                self.cur_obj = new_map
                self.cur_in = 'map'

            elif type_ == self.clib.MAPPING_END_EVENT:
                self.pop_state()

            elif type_ == self.clib.DOCUMENT_START_EVENT:
                pass
            elif type_ == self.clib.DOCUMENT_END_EVENT:
                self.clib.event_delete(event)
                break

            self.clib.event_delete(event)

        return self.last_obj


def loads(string, clib):
    parser = clib.new_parser()

    if not clib.parser_initialize(parser):
        raise YGPError('Failed to init libyaml')

    try:
        clib.parser_set_encoding(parser, clib.UTF8_ENCODING)
        clib.parser_set_input_string(parser, string, len(string))
        event_handler = YAMLEventHandler(clib, parser)
        return event_handler.process()
    finally:
        clib.parser_delete(parser)


def load(file_obj, clib):
    parser = clib.new_parser()

    if not clib.parser_initialize(parser):
        raise YGPError('Failed to init libyaml')

    try:
        clib.parser_set_encoding(parser, clib.UTF8_ENCODING)
        clib.parser_set_input_file(parser, file_obj)
        event_handler = YAMLEventHandler(clib, parser)
        return event_handler.process()
    finally:
        clib.parser_delete(parser)
