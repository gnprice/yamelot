from __future__ import absolute_import
from __future__ import print_function

from collections import OrderedDict, Mapping
import datetime
import re
import string


class YGPError(Exception):
    pass


class YGPValueError(YGPError, ValueError):
    pass


class YAMLEventHandler(object):
    DATE = re.compile(
        r'(?P<year>[0-9][0-9][0-9][0-9])'
        r'-'
        r'(?P<month>[0-9][0-9]?)'
        r'-'
        r'(?P<day>[0-9][0-9]?)'
        r'$'
    )
    DATETIME = re.compile(
        r'(?P<year>[0-9][0-9][0-9][0-9])'
        r'-'
        r'(?P<month>[0-9][0-9]?)'
        r'-'
        r'(?P<day>[0-9][0-9]?)'
        r'([Tt]|[ \t]+)'
        r'(?P<hour>[0-9][0-9]?)'
        r':'
        r'(?P<minute>[0-9][0-9]?)'
        r':'
        r'(?P<second>[0-9][0-9]?)'
        r'(\.'                      # optional fraction
        r'(?P<fraction>[0-9]*)'
        r')?'                       # end optional fraction
        r'('                        # optional timezone
        r'([ \t]*)Z'                # choice #1 Z
        r'|'                        # start choice #2 -/+ offset
        r'(?P<tz_direction>[-+])'
        r'(?P<tz_hour>[0-9][0-9]?)'
        r'('
        r':'
        r'(?P<tz_minute>[0-9][0-9])?'
        r')?'                       # end optional tz_minute
        r')?'                       # end optional timezone
        r'$'
    )
    BASE_10 = re.compile(
        r'[-+]?'
        r'(0|[1-9][0-9_]*)'
        r'$'
    )

    BASE_16 = re.compile(
        r'[-+]?'
        r'0x[0-9a-fA-F][0-9a-fA-F_]*'
        r'$'
    )
    FLOAT = re.compile(
        r'[-+]?'
        r'([0-9][0-9_]*)?'
        r'\.'
        r'[0-9]*'
        r'([eE][-+][0-9]+)?'
        r'$'
    )
    NUMBER_START = string.digits + '-+.'

    def __init__(self, clib, parser):
        self.clib = clib
        self.parser = parser
        self.stack = []
        self.anchors = {}

        self.cur_obj = None
        self.cur_in = False
        self.map_key = None
        self.non_merge_keys = None

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
            self.non_merge_keys,
        ))

    def pop_state(self):
        self.last_obj = self.cur_obj
        (
            self.cur_obj,
            self.cur_in,
            self.map_key,
            self.non_merge_keys,
        ) = self.stack.pop()

    def set_map(self, value):
        assert self.cur_in == 'map'
        assert self.map_key is not None
        if self.map_key == '<<':
            defaults = value
            if isinstance(defaults, Mapping):
                for key, merge_value in value.items():
                    # PyYAML says last wins
                    if key not in self.non_merge_keys:
                        self.cur_obj[key] = merge_value
            else:
                # The list merge spec says "first wins"
                merged_keys = self.non_merge_keys.copy()
                for default in defaults:
                    for key, merge_value in default.items():
                        if key not in merged_keys:
                            merged_keys.add(key)
                            self.cur_obj[key] = merge_value
        else:
            self.non_merge_keys.add(self.map_key)
            self.cur_obj[self.map_key] = value
        self.map_key = None

    def convert_scalar(self, value):
        if value.isdigit() and value[0] == '0' and len(value) > 1:
            raise self.build_custom_error(
                'Octal scalers are not supported {!r}'.format(value)
            )

        lower_value = value.lower()
        if lower_value == 'null':
            return None
        if lower_value in ('true', 'yes', 'on'):
            return True
        if lower_value in ('false', 'no', 'off'):
            return False

        if value == '':
            if self.cur_in == 'map' and self.map_key is not None:
                return None
            else:
                raise self.build_custom_error('Missing value')

        # This check saves .3 sec on my 3.4MB file
        if value[0] in self.NUMBER_START and value != '.':
            base_10 = self.BASE_10.match(value)
            if base_10:
                return int(base_10.group(0).replace('_', ''))

            base_16 = self.BASE_16.match(value)
            if base_16:
                return int(base_16.group(0).replace('_', ''), 16)

            float_ = self.FLOAT.match(value)
            if float_:
                return float(float_.group(0).replace('_', ''))

        # This check saves another .3 sec on my 3.4MB file
        if len(value) > 5 and value[4] == '-':
            date_match = self.DATE.match(value)
            if date_match:
                groups = date_match.groupdict()
                return datetime.date(
                    year=int(groups['year']),
                    month=int(groups['month']),
                    day=int(groups['day']),
                )

            datetime_match = self.DATETIME.match(value)
            if datetime_match:
                groups = datetime_match.groupdict()

                offset = datetime.timedelta(
                    hours=int(groups['tz_hour'] or 0),
                    minutes=int(groups['tz_minute'] or 0),
                )

                if groups.get('tz_direction', '+') == '+':
                    offset = offset * -1

                return datetime.datetime(
                    year=int(groups['year']),
                    month=int(groups['month']),
                    day=int(groups['day']),
                    hour=int(groups['hour']),
                    minute=int(groups['minute']),
                    second=int(groups['second']),
                    microsecond=int(groups['fraction'] or 0),
                ) + offset
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
        elif self.cur_in == 'map' and self.map_key is not None:
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

                value = self.clib.buffer(
                    scalar.value,
                    scalar.length,
                )[:].decode('utf-8')

                if scalar.style == self.clib.FOLDED_SCALAR_STYLE:
                    raise self.build_custom_error(
                        'Folded Scalars are not allowed {!r}'.format(
                            value
                        )
                    )

                if scalar.style == self.clib.PLAIN_SCALAR_STYLE:
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
                self.non_merge_keys = set()

            elif type_ == self.clib.MAPPING_END_EVENT:
                self.pop_state()

            elif type_ == self.clib.DOCUMENT_START_EVENT:
                pass
            elif type_ == self.clib.DOCUMENT_END_EVENT:
                self.clib.event_delete(event)
                break

            self.clib.event_delete(event)

        return self.last_obj


def loads(in_str, clib):
    parser = clib.new_parser()

    if not clib.parser_initialize(parser):
        raise YGPError('Failed to init libyaml')

    try:
        clib.parser_set_encoding(parser, clib.UTF8_ENCODING)
        in_str_p = clib.ffi.new('const char[]', in_str)
        clib.parser_set_input_string(parser, in_str_p, len(in_str))
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
        file_obj_p = clib.ffi.new('FILE *', file_obj)
        clib.parser_set_input_file(parser, file_obj_p)
        event_handler = YAMLEventHandler(clib, parser)
        return event_handler.process()
    finally:
        clib.parser_delete(parser)
