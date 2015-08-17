from cffi import FFI
ffi = FFI()


ffi.set_source(
    'ygp._yaml',
    """
    #include <yaml.h>
    """,
    libraries=['yaml'],
)

ffi.cdef("""

typedef enum yaml_error_type_e { ...  } yaml_error_type_t;

typedef enum yaml_encoding_e { ...  } yaml_encoding_t;

typedef enum yaml_sequence_style_e { ...  } yaml_sequence_style_t;

typedef enum yaml_mapping_style_e { ...  } yaml_mapping_style_t;

typedef enum yaml_scalar_style_e { ...  } yaml_scalar_style_t;

typedef enum yaml_event_type_e {
    YAML_DOCUMENT_START_EVENT,
    YAML_DOCUMENT_END_EVENT,

    YAML_SCALAR_EVENT,

    YAML_SEQUENCE_START_EVENT,
    YAML_SEQUENCE_END_EVENT,

    YAML_MAPPING_START_EVENT,
    YAML_MAPPING_END_EVENT,

    YAML_ALIAS_EVENT,

    ...
} yaml_event_type_t;


typedef unsigned char yaml_char_t;

typedef ... yaml_version_directive_t;

typedef ... yaml_tag_directive_t;

typedef struct yaml_mark_s {
    size_t line;
    size_t column;
    ...;
} yaml_mark_t;



typedef struct yaml_parser_s {
    yaml_error_type_t error;
    const char *problem;

    /** The problematic value (@c -1 is none). */
    int problem_value;
    yaml_mark_t problem_mark;

    const char *context;
    yaml_mark_t context_mark;
    ...;
} yaml_parser_t;

typedef struct yaml_event_s {
    yaml_event_type_t type;

    union {
        struct {
            yaml_encoding_t encoding;
        } stream_start;

        struct {
            yaml_version_directive_t *version_directive;

            struct {
                yaml_tag_directive_t *start;
                yaml_tag_directive_t *end;
            } tag_directives;

            int implicit;
        } document_start;

        struct {
            int implicit;
        } document_end;

        struct {
            yaml_char_t *anchor;
        } alias;

        struct {
            yaml_char_t *anchor;
            yaml_char_t *tag;
            yaml_char_t *value;
            size_t length;
            int plain_implicit;
            int quoted_implicit;
            yaml_scalar_style_t style;
        } scalar;

        struct {
            yaml_char_t *anchor;
            yaml_char_t *tag;
            int implicit;
            yaml_sequence_style_t style;
        } sequence_start;

        struct {
            yaml_char_t *anchor;
            yaml_char_t *tag;
            int implicit;
            yaml_mapping_style_t style;
        } mapping_start;

    } data;

    ...;

} yaml_event_t;

int yaml_parser_initialize(yaml_parser_t *parser);

void yaml_parser_delete(yaml_parser_t *parser);

void yaml_parser_set_input_string(yaml_parser_t *parser,
        const unsigned char *input, size_t size);

void yaml_parser_set_input_file(yaml_parser_t *parser, FILE *file);

int yaml_parser_parse(yaml_parser_t *parser, yaml_event_t *event);

void yaml_event_delete(yaml_event_t *event);

""")

if __name__ == "__main__":
    ffi.compile()
