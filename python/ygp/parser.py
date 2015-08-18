from __future__ import absolute_import
from __future__ import print_function

from collections import OrderedDict

from ygp._yaml import ffi, lib


class YGPError(Exception):
    pass


class YGPValueError(YGPError, ValueError):
    pass


class YAMLEventHandler(object):

    def __init__(self, parser):
        self.parser = parser
        self.stack = []
        self.anchors = {}

        self.cur_obj = None
        self.cur_in = False
        self.map_key = None

        self.last_obj = None

    def build_libyaml_error(self):
        buf = []
        if self.parser.problem != ffi.NULL:
            buf.append(ffi.string(self.parser.problem))

            buf.append(
                'at line: {} col: {}'.format(
                    self.parser.problem_mark.line + 1,
                    self.parser.problem_mark.column + 1,
                )
            )

        if self.parser.context != ffi.NULL:
            buf.append(ffi.string(self.parser.context))

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
            for key, value in defaults.items():
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
        if anchor != ffi.NULL:
            anchor_value = ffi.string(anchor)
            if anchor_value in self.anchors:
                raise self.build_custom_error(
                    'Anchor {} already in use'.format(anchor_value)
                )
            self.anchors[anchor_value] = value

    def check_tag(self, tag):
        if tag != ffi.NULL:
            raise self.build_custom_error(
                'Tags are not allowed tag={!}'.format(ffi.string(tag))
            )

    def process(self):
        event = ffi.new('yaml_event_t *')

        while True:
            if not lib.yaml_parser_parse(self.parser, event):
                lib.yaml_event_delete(event)
                raise self.build_libyaml_error()

            type_ = event.type

            if type_ == lib.YAML_SCALAR_EVENT:

                value = ffi.string(
                    event.data.scalar.value,
                    event.data.scalar.length,
                )

                if event.data.scalar.style == lib.YAML_FOLDED_SCALAR_STYLE:
                    raise self.build_custom_error(
                        'Folded Scalars are not alowed {!r}'.format(
                            value
                        )
                    )

                if (
                    value == '' and
                    event.data.scalar.style == lib.YAML_PLAIN_SCALAR_STYLE
                ):
                    raise self.build_custom_error(
                        'Missing value'
                    )

                value = self.convert_scalar(value)
                self.add_anchor(event.data.scalar.anchor, value)
                self.check_tag(event.data.scalar.tag)

                if self.cur_in == 'seq':
                    self.cur_obj.append(value)
                elif self.cur_in == 'map' and self.map_key is None:
                    self.map_key = value
                else:
                    self.set_map(value)

            elif type_ == lib.YAML_ALIAS_EVENT:
                anchor = ffi.string(event.data.alias.anchor)
                try:
                    anchor_value = self.anchors[anchor]
                except KeyError:
                    raise self.build_custom_error(
                        'Anchor %s not found' % (
                            ffi.string(event.data.alias.anchor),
                        )
                    )
                if self.cur_in == 'seq':
                    self.cur_obj.append(anchor_value)
                elif self.cur_in == 'map' and self.map_key is None:
                    self.map_key = anchor_value
                else:
                    self.set_map(anchor_value)

            elif type_ == lib.YAML_SEQUENCE_START_EVENT:
                new_seq = []
                self.add_anchor(event.data.sequence_start.anchor, new_seq)
                self.check_tag(event.data.sequence_start.tag)
                if self.cur_in == 'map':
                    self.set_map(new_seq)
                self.push_state()
                self.cur_obj = new_seq
                self.cur_in = 'seq'

            elif type_ == lib.YAML_SEQUENCE_END_EVENT:
                self.pop_state()

            elif type_ == lib.YAML_MAPPING_START_EVENT:
                new_map = OrderedDict()
                self.add_anchor(event.data.mapping_start.anchor, new_map)
                self.check_tag(event.data.mapping_start.tag)

                if self.cur_in == 'map':
                    self.set_map(new_map)
                self.push_state()
                self.cur_obj = new_map
                self.cur_in = 'map'

            elif type_ == lib.YAML_MAPPING_END_EVENT:
                self.pop_state()

            elif type_ == lib.YAML_DOCUMENT_START_EVENT:
                pass
            elif type_ == lib.YAML_DOCUMENT_END_EVENT:
                lib.yaml_event_delete(event)
                break

            lib.yaml_event_delete(event)

        return self.last_obj


def loads(string):
    parser = ffi.new('yaml_parser_t *')

    if not lib.yaml_parser_initialize(parser):
        raise YGPError('Failed to init libyaml')

    try:
        lib.yaml_parser_set_input_string(parser, string, len(string))
        event_handler = YAMLEventHandler(parser)
        return event_handler.process()
    finally:
        lib.yaml_parser_delete(parser)


def load(file_obj):
    parser = ffi.new('yaml_parser_t *')

    if not lib.yaml_parser_initialize(parser):
        raise YGPError('Failed to init libyaml')

    try:
        lib.yaml_parser_set_input_file(parser, file_obj)
        event_handler = YAMLEventHandler(parser)
        return event_handler.process()
    finally:
        lib.yaml_parser_delete(parser)
