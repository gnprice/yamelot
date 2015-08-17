from __future__ import absolute_import
from __future__ import print_function

from collections import OrderedDict

from ygp._yaml import ffi, lib


class YAMLError(ValueError):
    pass


def raise_error(parser):
    buf = []
    if parser.problem != ffi.NULL:
        buf.append(ffi.string(parser.problem))

        buf.append(
            'at line: {} col: {}'.format(
                parser.problem_mark.line + 1,
                parser.problem_mark.column + 1,
            )
        )

    if parser.context != ffi.NULL:
        buf.append(ffi.string(parser.context))

        buf.append(
            'at line: {} col: {}'.format(
                parser.context_mark.line + 1,
                parser.context_mark.column + 1,
            )
        )

    raise YAMLError(' '.join(buf))


class YAMLEventHandler(object):

    def __init__(self):
        self.stack = []
        self.anchors = {}

        self.cur_obj = None
        self.cur_in = False
        self.map_key = None

        self.last_obj = None

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
                raise YAMLError()
            self.anchors[anchor_value] = value

    def process(self, parser):
        event = ffi.new('yaml_event_t *')

        while True:
            if not lib.yaml_parser_parse(parser, event):
                lib.yaml_event_delete(event)
                raise_error(parser)

            type_ = event.type

            if type_ == lib.YAML_SCALAR_EVENT:
                value = ffi.string(
                    event.data.scalar.value,
                    event.data.scalar.length,
                )
                value = self.convert_scalar(value)
                self.add_anchor(event.data.scalar.anchor, value)

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
                    raise YAMLError(
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
                self.add_anchor(event.data.scalar.anchor, new_seq)
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
        raise Exception('Failed to init')

    try:
        lib.yaml_parser_set_input_string(parser, string, len(string))
        event_handler = YAMLEventHandler()
        return event_handler.process(parser)
    finally:
        lib.yaml_parser_delete(parser)


def load(file_obj):
    parser = ffi.new('yaml_parser_t *')

    if not lib.yaml_parser_initialize(parser):
        raise Exception('Failed to init')

    try:
        lib.yaml_parser_set_input_file(parser, file_obj)
        event_handler = YAMLEventHandler()
        return event_handler.process(parser)
    finally:
        lib.yaml_parser_delete(parser)
