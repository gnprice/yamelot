#!/usr/bin/env python
from cffi import FFI
ffi = FFI()

C_HEADER_SRC = '#include <ygp.h>'
C_KEYWORDS = {
    'libraries': ['ygp'],
}

if hasattr(ffi, 'set_source'):
    ffi.set_source(
        'ygp.clib._ygp',
        C_HEADER_SRC,
        **C_KEYWORDS
    )

ffi.cdef("""

typedef enum ygp_error_type_e { ...  } ygp_error_type_t;

typedef enum ygp_encoding_e {
    YGP_UTF8_ENCODING,

    ...
} ygp_encoding_t;

typedef enum ygp_sequence_style_e { ...  } ygp_sequence_style_t;

typedef enum ygp_mapping_style_e { ...  } ygp_mapping_style_t;

typedef enum ygp_scalar_style_e {
    YGP_PLAIN_SCALAR_STYLE,
    YGP_LITERAL_SCALAR_STYLE,
    YGP_FOLDED_SCALAR_STYLE,

    ...
} ygp_scalar_style_t;

typedef enum ygp_event_type_e {
    YGP_DOCUMENT_START_EVENT,
    YGP_DOCUMENT_END_EVENT,

    YGP_SCALAR_EVENT,

    YGP_SEQUENCE_START_EVENT,
    YGP_SEQUENCE_END_EVENT,

    YGP_MAPPING_START_EVENT,
    YGP_MAPPING_END_EVENT,

    YGP_ALIAS_EVENT,

    ...
} ygp_event_type_t;

typedef enum ygp_scalar_type_e {
    YGP_TYPE_BOOL,
    YGP_TYPE_NULL,
    YGP_TYPE_NUMBERISH,

    ...
} ygp_scalar_type_t;


typedef unsigned char ygp_char_t;

typedef ... ygp_version_directive_t;

typedef ... ygp_tag_directive_t;

typedef struct ygp_mark_s {
    size_t line;
    size_t column;
    ...;
} ygp_mark_t;

typedef struct ygp_parser_s {
    ygp_error_type_t error;
    const char *problem;

    /** The problematic value (@c -1 is none). */
    int problem_value;
    ygp_mark_t problem_mark;

    const char *context;
    ygp_mark_t context_mark;

    ygp_mark_t mark;
    ...;
} ygp_parser_t;

typedef struct ygp_event_s {
    ygp_event_type_t type;

    union {
        struct {
            ygp_encoding_t encoding;
        } stream_start;

        struct {
            ygp_version_directive_t *version_directive;

            struct {
                ygp_tag_directive_t *start;
                ygp_tag_directive_t *end;
            } tag_directives;

            int implicit;
        } document_start;

        struct {
            int implicit;
        } document_end;

        struct {
            ygp_char_t *anchor;
        } alias;

        struct {
            ygp_char_t *anchor;
            ygp_char_t *tag;
            ygp_char_t *value;
            size_t length;
            int plain_implicit;
            int quoted_implicit;
            ygp_scalar_style_t style;
            ygp_scalar_type_t type;
        } scalar;

        struct {
            ygp_char_t *anchor;
            ygp_char_t *tag;
            int implicit;
            ygp_sequence_style_t style;
        } sequence_start;

        struct {
            ygp_char_t *anchor;
            ygp_char_t *tag;
            int implicit;
            ygp_mapping_style_t style;
        } mapping_start;

    } data;

    ...;

} ygp_event_t;

int ygp_parser_initialize(ygp_parser_t *parser);

void ygp_parser_delete(ygp_parser_t *parser);

void ygp_parser_set_encoding(ygp_parser_t *parser, ygp_encoding_t encoding);

void ygp_parser_set_input_string(ygp_parser_t *parser,
        const unsigned char *input, size_t size);

void ygp_parser_set_input_file(ygp_parser_t *parser, FILE *file);

int ygp_parser_parse(ygp_parser_t *parser, ygp_event_t *event);

void ygp_event_delete(ygp_event_t *event);

""")

if __name__ == "__main__":
    ffi.compile()
