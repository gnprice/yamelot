from __future__ import absolute_import
from __future__ import print_function

try:
    from ygp.clib._ygp import ffi, lib
except ImportError:
    from ygp.clib.ygp_build import ffi, C_HEADER_SRC, C_KEYWORDS
    lib = ffi.verify(C_HEADER_SRC, **C_KEYWORDS)

__all__ = [
    'ffi',
    'NULL',
    'buffer',
    'string',
    'new_event',
    'new_parser',
    'parser_parse',
    'event_delete',
    'parser_initialize',
    'parser_set_encoding',
    'parser_set_input_string',
    'parser_set_input_file',
    'parser_delete',
    'UTF8_ENCODING',
    'PLAIN_SCALAR_STYLE',
    'LITERAL_SCALAR_STYLE',
    'FOLDED_SCALAR_STYLE',
    'DOCUMENT_START_EVENT',
    'DOCUMENT_END_EVENT',
    'SCALAR_EVENT',
    'SEQUENCE_START_EVENT',
    'SEQUENCE_END_EVENT',
    'MAPPING_START_EVENT',
    'MAPPING_END_EVENT',
    'ALIAS_EVENT',
    'TYPE_BOOL',
    'TYPE_NULL',
]

prefix = 'ygp_'
PREFIX = 'YGP_'


NULL = ffi.NULL
buffer = ffi.buffer
string = ffi.string


def new_event():
    return ffi.new(prefix + 'event_t *')


def new_parser():
    return ffi.new(prefix + 'parser_t *')


parser_parse = getattr(lib, prefix + 'parser_parse')
event_delete = getattr(lib, prefix + 'event_delete')
parser_initialize = getattr(lib, prefix + 'parser_initialize')
parser_set_encoding = getattr(lib, prefix + 'parser_set_encoding')
parser_set_input_string = getattr(lib, prefix + 'parser_set_input_string')
parser_set_input_file = getattr(lib, prefix + 'parser_set_input_file')
parser_delete = getattr(lib, prefix + 'parser_delete')

UTF8_ENCODING = getattr(lib, PREFIX + 'UTF8_ENCODING')
PLAIN_SCALAR_STYLE = getattr(lib, PREFIX + 'PLAIN_SCALAR_STYLE')
LITERAL_SCALAR_STYLE = getattr(lib, PREFIX + 'LITERAL_SCALAR_STYLE')
FOLDED_SCALAR_STYLE = getattr(lib, PREFIX + 'FOLDED_SCALAR_STYLE')
DOCUMENT_START_EVENT = getattr(lib, PREFIX + 'DOCUMENT_START_EVENT')
DOCUMENT_END_EVENT = getattr(lib, PREFIX + 'DOCUMENT_END_EVENT')
SCALAR_EVENT = getattr(lib, PREFIX + 'SCALAR_EVENT')
SEQUENCE_START_EVENT = getattr(lib, PREFIX + 'SEQUENCE_START_EVENT')
SEQUENCE_END_EVENT = getattr(lib, PREFIX + 'SEQUENCE_END_EVENT')
MAPPING_START_EVENT = getattr(lib, PREFIX + 'MAPPING_START_EVENT')
MAPPING_END_EVENT = getattr(lib, PREFIX + 'MAPPING_END_EVENT')
ALIAS_EVENT = getattr(lib, PREFIX + 'ALIAS_EVENT')
TYPE_BOOL = getattr(lib, PREFIX + 'TYPE_BOOL')
TYPE_NULL = getattr(lib, PREFIX + 'TYPE_NULL')
