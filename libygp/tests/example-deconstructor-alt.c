
#include <ygp.h>

#include <stdlib.h>
#include <stdio.h>

int
main(int argc, char *argv[])
{
    int help = 0;
    int canonical = 0;
    int unicode = 0;
    int k;
    int done = 0;

    ygp_parser_t parser;
    ygp_emitter_t emitter;
    ygp_event_t input_event;
    ygp_document_t output_document;

    int root;

    /* Clear the objects. */

    memset(&parser, 0, sizeof(parser));
    memset(&emitter, 0, sizeof(emitter));
    memset(&input_event, 0, sizeof(input_event));
    memset(&output_document, 0, sizeof(output_document));

    /* Analyze command line options. */

    for (k = 1; k < argc; k ++)
    {
        if (strcmp(argv[k], "-h") == 0
                || strcmp(argv[k], "--help") == 0) {
            help = 1;
        }

        else if (strcmp(argv[k], "-c") == 0
                || strcmp(argv[k], "--canonical") == 0) {
            canonical = 1;
        }

        else if (strcmp(argv[k], "-u") == 0
                || strcmp(argv[k], "--unicode") == 0) {
            unicode = 1;
        }

        else {
            fprintf(stderr, "Unrecognized option: %s\n"
                    "Try `%s --help` for more information.\n",
                    argv[k], argv[0]);
            return 1;
        }
    }

    /* Display the help string. */

    if (help)
    {
        printf("%s <input\n"
                "or\n%s -h | --help\nDeconstruct a YAML stream\n\nOptions:\n"
                "-h, --help\t\tdisplay this help and exit\n"
                "-c, --canonical\t\toutput in the canonical YAML format\n"
                "-u, --unicode\t\toutput unescaped non-ASCII characters\n",
                argv[0], argv[0]);
        return 0;
    }

    /* Initialize the parser and emitter objects. */

    if (!ygp_parser_initialize(&parser)) {
        fprintf(stderr, "Could not initialize the parser object\n");
        return 1;
    }

    if (!ygp_emitter_initialize(&emitter)) {
        ygp_parser_delete(&parser);
        fprintf(stderr, "Could not inialize the emitter object\n");
        return 1;
    }

    /* Set the parser parameters. */

    ygp_parser_set_input_file(&parser, stdin);

    /* Set the emitter parameters. */

    ygp_emitter_set_output_file(&emitter, stdout);

    ygp_emitter_set_canonical(&emitter, canonical);
    ygp_emitter_set_unicode(&emitter, unicode);

    /* Create and emit the STREAM-START event. */

    if (!ygp_emitter_open(&emitter))
        goto emitter_error;

    /* Create a output_document object. */

    if (!ygp_document_initialize(&output_document, NULL, NULL, NULL, 0, 0))
        goto document_error;

    /* Create the root sequence. */

    root = ygp_document_add_sequence(&output_document, NULL,
            YGP_BLOCK_SEQUENCE_STYLE);
    if (!root) goto document_error;

    /* Loop through the input events. */

    while (!done)
    {
        int properties, key, value, map, seq;

        /* Get the next event. */

        if (!ygp_parser_parse(&parser, &input_event))
            goto parser_error;

        /* Check if this is the stream end. */

        if (input_event.type == YGP_STREAM_END_EVENT) {
            done = 1;
        }

        /* Create a mapping node and attach it to the root sequence. */

        properties = ygp_document_add_mapping(&output_document, NULL,
                YGP_BLOCK_MAPPING_STYLE);
        if (!properties) goto document_error;
        if (!ygp_document_append_sequence_item(&output_document,
                    root, properties)) goto document_error;

        /* Analyze the event. */

        switch (input_event.type)
        {
            case YGP_STREAM_START_EVENT:

                /* Add 'type': 'STREAM-START'. */

                key = ygp_document_add_scalar(&output_document, NULL,
                    "type", -1, YGP_PLAIN_SCALAR_STYLE);
                if (!key) goto document_error;
                value = ygp_document_add_scalar(&output_document, NULL,
                    "STREAM-START", -1, YGP_PLAIN_SCALAR_STYLE);
                if (!value) goto document_error;
                if (!ygp_document_append_mapping_pair(&output_document,
                            properties, key, value)) goto document_error;

                /* Add 'encoding': <encoding>. */

                if (input_event.data.stream_start.encoding)
                {
                    ygp_encoding_t encoding
                        = input_event.data.stream_start.encoding;

                    key = ygp_document_add_scalar(&output_document, NULL,
                        "encoding", -1, YGP_PLAIN_SCALAR_STYLE);
                    if (!key) goto document_error;
                    value = ygp_document_add_scalar(&output_document, NULL,
                            (encoding == YGP_UTF8_ENCODING ? "utf-8" :
                             encoding == YGP_UTF16LE_ENCODING ? "utf-16-le" :
                             encoding == YGP_UTF16BE_ENCODING ? "utf-16-be" :
                             "unknown"), -1, YGP_PLAIN_SCALAR_STYLE);
                    if (!value) goto document_error;
                    if (!ygp_document_append_mapping_pair(&output_document,
                                properties, key, value)) goto document_error;
                }
                    
                break;

            case YGP_STREAM_END_EVENT:

                /* Add 'type': 'STREAM-END'. */

                key = ygp_document_add_scalar(&output_document, NULL,
                    "type", -1, YGP_PLAIN_SCALAR_STYLE);
                if (!key) goto document_error;
                value = ygp_document_add_scalar(&output_document, NULL,
                    "STREAM-END", -1, YGP_PLAIN_SCALAR_STYLE);
                if (!value) goto document_error;
                if (!ygp_document_append_mapping_pair(&output_document,
                            properties, key, value)) goto document_error;

                break;

            case YGP_DOCUMENT_START_EVENT:

                /* Add 'type': 'DOCUMENT-START'. */

                key = ygp_document_add_scalar(&output_document, NULL,
                    "type", -1, YGP_PLAIN_SCALAR_STYLE);
                if (!key) goto document_error;
                value = ygp_document_add_scalar(&output_document, NULL,
                    "DOCUMENT-START", -1, YGP_PLAIN_SCALAR_STYLE);
                if (!value) goto document_error;
                if (!ygp_document_append_mapping_pair(&output_document,
                            properties, key, value)) goto document_error;

                /* Display the output_document version numbers. */

                if (input_event.data.document_start.version_directive)
                {
                    ygp_version_directive_t *version
                        = input_event.data.document_start.version_directive;
                    char number[64];

                    /* Add 'version': {}. */
                    
                    key = ygp_document_add_scalar(&output_document, NULL,
                        "version", -1, YGP_PLAIN_SCALAR_STYLE);
                    if (!key) goto document_error;
                    map = ygp_document_add_mapping(&output_document, NULL,
                            YGP_FLOW_MAPPING_STYLE);
                    if (!map) goto document_error;
                    if (!ygp_document_append_mapping_pair(&output_document,
                                properties, key, map)) goto document_error;

                    /* Add 'major': <number>. */

                    key = ygp_document_add_scalar(&output_document, NULL,
                        "major", -1, YGP_PLAIN_SCALAR_STYLE);
                    if (!key) goto document_error;
                    sprintf(number, "%d", version->major);
                    value = ygp_document_add_scalar(&output_document, YGP_INT_TAG,
                        number, -1, YGP_PLAIN_SCALAR_STYLE);
                    if (!value) goto document_error;
                    if (!ygp_document_append_mapping_pair(&output_document,
                                map, key, value)) goto document_error;

                    /* Add 'minor': <number>. */

                    key = ygp_document_add_scalar(&output_document, NULL,
                        "minor", -1, YGP_PLAIN_SCALAR_STYLE);
                    if (!key) goto document_error;
                    sprintf(number, "%d", version->minor);
                    value = ygp_document_add_scalar(&output_document, YGP_INT_TAG,
                        number, -1, YGP_PLAIN_SCALAR_STYLE);
                    if (!value) goto document_error;
                    if (!ygp_document_append_mapping_pair(&output_document,
                                map, key, value)) goto document_error;
                }

                /* Display the output_document tag directives. */

                if (input_event.data.document_start.tag_directives.start
                        != input_event.data.document_start.tag_directives.end)
                {
                    ygp_tag_directive_t *tag;

                    /* Add 'tags': []. */
                    
                    key = ygp_document_add_scalar(&output_document, NULL,
                        "tags", -1, YGP_PLAIN_SCALAR_STYLE);
                    if (!key) goto document_error;
                    seq = ygp_document_add_sequence(&output_document, NULL,
                            YGP_BLOCK_SEQUENCE_STYLE);
                    if (!seq) goto document_error;
                    if (!ygp_document_append_mapping_pair(&output_document,
                                properties, key, seq)) goto document_error;

                    for (tag = input_event.data.document_start.tag_directives.start;
                            tag != input_event.data.document_start.tag_directives.end;
                            tag ++)
                    {
                        /* Add {}. */

                        map = ygp_document_add_mapping(&output_document, NULL,
                                YGP_FLOW_MAPPING_STYLE);
                        if (!map) goto document_error;
                        if (!ygp_document_append_sequence_item(&output_document,
                                    seq, map)) goto document_error;

                        /* Add 'handle': <handle>. */

                        key = ygp_document_add_scalar(&output_document, NULL,
                            "handle", -1, YGP_PLAIN_SCALAR_STYLE);
                        if (!key) goto document_error;
                        value = ygp_document_add_scalar(&output_document, NULL,
                            tag->handle, -1, YGP_DOUBLE_QUOTED_SCALAR_STYLE);
                        if (!value) goto document_error;
                        if (!ygp_document_append_mapping_pair(&output_document,
                                    map, key, value)) goto document_error;

                        /* Add 'prefix': <prefix>. */

                        key = ygp_document_add_scalar(&output_document, NULL,
                            "prefix", -1, YGP_PLAIN_SCALAR_STYLE);
                        if (!key) goto document_error;
                        value = ygp_document_add_scalar(&output_document, NULL,
                            tag->prefix, -1, YGP_DOUBLE_QUOTED_SCALAR_STYLE);
                        if (!value) goto document_error;
                        if (!ygp_document_append_mapping_pair(&output_document,
                                    map, key, value)) goto document_error;
                    }
                }

                /* Add 'implicit': <flag>. */

                key = ygp_document_add_scalar(&output_document, NULL,
                    "implicit", -1, YGP_PLAIN_SCALAR_STYLE);
                if (!key) goto document_error;
                value = ygp_document_add_scalar(&output_document, YGP_BOOL_TAG,
                        (input_event.data.document_start.implicit ?
                         "true" : "false"), -1, YGP_PLAIN_SCALAR_STYLE);
                if (!value) goto document_error;
                if (!ygp_document_append_mapping_pair(&output_document,
                            properties, key, value)) goto document_error;

                break;

            case YGP_DOCUMENT_END_EVENT:

                /* Add 'type': 'DOCUMENT-END'. */

                key = ygp_document_add_scalar(&output_document, NULL,
                    "type", -1, YGP_PLAIN_SCALAR_STYLE);
                if (!key) goto document_error;
                value = ygp_document_add_scalar(&output_document, NULL,
                    "DOCUMENT-END", -1, YGP_PLAIN_SCALAR_STYLE);
                if (!value) goto document_error;
                if (!ygp_document_append_mapping_pair(&output_document,
                            properties, key, value)) goto document_error;

                /* Add 'implicit': <flag>. */

                key = ygp_document_add_scalar(&output_document, NULL,
                    "implicit", -1, YGP_PLAIN_SCALAR_STYLE);
                if (!key) goto document_error;
                value = ygp_document_add_scalar(&output_document, YGP_BOOL_TAG,
                        (input_event.data.document_end.implicit ?
                         "true" : "false"), -1, YGP_PLAIN_SCALAR_STYLE);
                if (!value) goto document_error;
                if (!ygp_document_append_mapping_pair(&output_document,
                            properties, key, value)) goto document_error;

                break;

            case YGP_ALIAS_EVENT:

                /* Add 'type': 'ALIAS'. */

                key = ygp_document_add_scalar(&output_document, NULL,
                    "type", -1, YGP_PLAIN_SCALAR_STYLE);
                if (!key) goto document_error;
                value = ygp_document_add_scalar(&output_document, NULL,
                    "ALIAS", -1, YGP_PLAIN_SCALAR_STYLE);
                if (!value) goto document_error;
                if (!ygp_document_append_mapping_pair(&output_document,
                            properties, key, value)) goto document_error;

                /* Add 'anchor': <anchor>. */

                key = ygp_document_add_scalar(&output_document, NULL,
                    "anchor", -1, YGP_PLAIN_SCALAR_STYLE);
                if (!key) goto document_error;
                value = ygp_document_add_scalar(&output_document, NULL,
                        input_event.data.alias.anchor, -1,
                        YGP_DOUBLE_QUOTED_SCALAR_STYLE);
                if (!value) goto document_error;
                if (!ygp_document_append_mapping_pair(&output_document,
                            properties, key, value)) goto document_error;

                break;

            case YGP_SCALAR_EVENT:

                /* Add 'type': 'SCALAR'. */

                key = ygp_document_add_scalar(&output_document, NULL,
                    "type", -1, YGP_PLAIN_SCALAR_STYLE);
                if (!key) goto document_error;
                value = ygp_document_add_scalar(&output_document, NULL,
                    "SCALAR", -1, YGP_PLAIN_SCALAR_STYLE);
                if (!value) goto document_error;
                if (!ygp_document_append_mapping_pair(&output_document,
                            properties, key, value)) goto document_error;

                /* Add 'anchor': <anchor>. */

                if (input_event.data.scalar.anchor)
                {
                    key = ygp_document_add_scalar(&output_document, NULL,
                        "anchor", -1, YGP_PLAIN_SCALAR_STYLE);
                    if (!key) goto document_error;
                    value = ygp_document_add_scalar(&output_document, NULL,
                            input_event.data.scalar.anchor, -1,
                            YGP_DOUBLE_QUOTED_SCALAR_STYLE);
                    if (!value) goto document_error;
                    if (!ygp_document_append_mapping_pair(&output_document,
                                properties, key, value)) goto document_error;
                }

                /* Add 'tag': <tag>. */

                if (input_event.data.scalar.tag)
                {
                    key = ygp_document_add_scalar(&output_document, NULL,
                        "tag", -1, YGP_PLAIN_SCALAR_STYLE);
                    if (!key) goto document_error;
                    value = ygp_document_add_scalar(&output_document, NULL,
                            input_event.data.scalar.tag, -1,
                            YGP_DOUBLE_QUOTED_SCALAR_STYLE);
                    if (!value) goto document_error;
                    if (!ygp_document_append_mapping_pair(&output_document,
                                properties, key, value)) goto document_error;
                }

                /* Add 'value': <value>. */

                key = ygp_document_add_scalar(&output_document, NULL,
                    "value", -1, YGP_PLAIN_SCALAR_STYLE);
                if (!key) goto document_error;
                value = ygp_document_add_scalar(&output_document, NULL,
                        input_event.data.scalar.value,
                        input_event.data.scalar.length,
                        YGP_DOUBLE_QUOTED_SCALAR_STYLE);
                if (!value) goto document_error;
                if (!ygp_document_append_mapping_pair(&output_document,
                            properties, key, value)) goto document_error;

                /* Display if the scalar tag is implicit. */

                /* Add 'implicit': {} */

                key = ygp_document_add_scalar(&output_document, NULL,
                    "version", -1, YGP_PLAIN_SCALAR_STYLE);
                if (!key) goto document_error;
                map = ygp_document_add_mapping(&output_document, NULL,
                        YGP_FLOW_MAPPING_STYLE);
                if (!map) goto document_error;
                if (!ygp_document_append_mapping_pair(&output_document,
                            properties, key, map)) goto document_error;

                /* Add 'plain': <flag>. */

                key = ygp_document_add_scalar(&output_document, NULL,
                    "plain", -1, YGP_PLAIN_SCALAR_STYLE);
                if (!key) goto document_error;
                value = ygp_document_add_scalar(&output_document, YGP_BOOL_TAG,
                        (input_event.data.scalar.plain_implicit ?
                         "true" : "false"), -1, YGP_PLAIN_SCALAR_STYLE);
                if (!value) goto document_error;
                if (!ygp_document_append_mapping_pair(&output_document,
                            map, key, value)) goto document_error;

                /* Add 'quoted': <flag>. */

                key = ygp_document_add_scalar(&output_document, NULL,
                    "quoted", -1, YGP_PLAIN_SCALAR_STYLE);
                if (!key) goto document_error;
                value = ygp_document_add_scalar(&output_document, YGP_BOOL_TAG,
                        (input_event.data.scalar.quoted_implicit ?
                         "true" : "false"), -1, YGP_PLAIN_SCALAR_STYLE);
                if (!value) goto document_error;
                if (!ygp_document_append_mapping_pair(&output_document,
                            map, key, value)) goto document_error;

                /* Display the style information. */

                if (input_event.data.scalar.style)
                {
                    ygp_scalar_style_t style = input_event.data.scalar.style;

                    /* Add 'style': <style>. */

                    key = ygp_document_add_scalar(&output_document, NULL,
                        "style", -1, YGP_PLAIN_SCALAR_STYLE);
                    if (!key) goto document_error;
                    value = ygp_document_add_scalar(&output_document, NULL,
                            (style == YGP_PLAIN_SCALAR_STYLE ? "plain" :
                             style == YGP_SINGLE_QUOTED_SCALAR_STYLE ?
                                    "single-quoted" :
                             style == YGP_DOUBLE_QUOTED_SCALAR_STYLE ?
                                    "double-quoted" :
                             style == YGP_LITERAL_SCALAR_STYLE ? "literal" :
                             style == YGP_FOLDED_SCALAR_STYLE ? "folded" :
                             "unknown"), -1, YGP_PLAIN_SCALAR_STYLE);
                    if (!value) goto document_error;
                    if (!ygp_document_append_mapping_pair(&output_document,
                                properties, key, value)) goto document_error;
                }

                break;

            case YGP_SEQUENCE_START_EVENT:

                /* Add 'type': 'SEQUENCE-START'. */

                key = ygp_document_add_scalar(&output_document, NULL,
                    "type", -1, YGP_PLAIN_SCALAR_STYLE);
                if (!key) goto document_error;
                value = ygp_document_add_scalar(&output_document, NULL,
                    "SEQUENCE-START", -1, YGP_PLAIN_SCALAR_STYLE);
                if (!value) goto document_error;
                if (!ygp_document_append_mapping_pair(&output_document,
                            properties, key, value)) goto document_error;

                /* Add 'anchor': <anchor>. */

                if (input_event.data.sequence_start.anchor)
                {
                    key = ygp_document_add_scalar(&output_document, NULL,
                        "anchor", -1, YGP_PLAIN_SCALAR_STYLE);
                    if (!key) goto document_error;
                    value = ygp_document_add_scalar(&output_document, NULL,
                            input_event.data.sequence_start.anchor, -1,
                            YGP_DOUBLE_QUOTED_SCALAR_STYLE);
                    if (!value) goto document_error;
                    if (!ygp_document_append_mapping_pair(&output_document,
                                properties, key, value)) goto document_error;
                }

                /* Add 'tag': <tag>. */

                if (input_event.data.sequence_start.tag)
                {
                    key = ygp_document_add_scalar(&output_document, NULL,
                        "tag", -1, YGP_PLAIN_SCALAR_STYLE);
                    if (!key) goto document_error;
                    value = ygp_document_add_scalar(&output_document, NULL,
                            input_event.data.sequence_start.tag, -1,
                            YGP_DOUBLE_QUOTED_SCALAR_STYLE);
                    if (!value) goto document_error;
                    if (!ygp_document_append_mapping_pair(&output_document,
                                properties, key, value)) goto document_error;
                }

                /* Add 'implicit': <flag>. */

                key = ygp_document_add_scalar(&output_document, NULL,
                    "implicit", -1, YGP_PLAIN_SCALAR_STYLE);
                if (!key) goto document_error;
                value = ygp_document_add_scalar(&output_document, YGP_BOOL_TAG,
                        (input_event.data.sequence_start.implicit ?
                         "true" : "false"), -1, YGP_PLAIN_SCALAR_STYLE);
                if (!value) goto document_error;
                if (!ygp_document_append_mapping_pair(&output_document,
                            properties, key, value)) goto document_error;

                /* Display the style information. */

                if (input_event.data.sequence_start.style)
                {
                    ygp_sequence_style_t style
                        = input_event.data.sequence_start.style;

                    /* Add 'style': <style>. */

                    key = ygp_document_add_scalar(&output_document, NULL,
                        "style", -1, YGP_PLAIN_SCALAR_STYLE);
                    if (!key) goto document_error;
                    value = ygp_document_add_scalar(&output_document, NULL,
                            (style == YGP_BLOCK_SEQUENCE_STYLE ? "block" :
                             style == YGP_FLOW_SEQUENCE_STYLE ? "flow" :
                             "unknown"), -1, YGP_PLAIN_SCALAR_STYLE);
                    if (!value) goto document_error;
                    if (!ygp_document_append_mapping_pair(&output_document,
                                properties, key, value)) goto document_error;
                }

                break;

            case YGP_SEQUENCE_END_EVENT:

                /* Add 'type': 'SEQUENCE-END'. */

                key = ygp_document_add_scalar(&output_document, NULL,
                    "type", -1, YGP_PLAIN_SCALAR_STYLE);
                if (!key) goto document_error;
                value = ygp_document_add_scalar(&output_document, NULL,
                    "SEQUENCE-END", -1, YGP_PLAIN_SCALAR_STYLE);
                if (!value) goto document_error;
                if (!ygp_document_append_mapping_pair(&output_document,
                            properties, key, value)) goto document_error;

                break;

            case YGP_MAPPING_START_EVENT:

                /* Add 'type': 'MAPPING-START'. */

                key = ygp_document_add_scalar(&output_document, NULL,
                    "type", -1, YGP_PLAIN_SCALAR_STYLE);
                if (!key) goto document_error;
                value = ygp_document_add_scalar(&output_document, NULL,
                    "MAPPING-START", -1, YGP_PLAIN_SCALAR_STYLE);
                if (!value) goto document_error;
                if (!ygp_document_append_mapping_pair(&output_document,
                            properties, key, value)) goto document_error;

                /* Add 'anchor': <anchor>. */

                if (input_event.data.mapping_start.anchor)
                {
                    key = ygp_document_add_scalar(&output_document, NULL,
                        "anchor", -1, YGP_PLAIN_SCALAR_STYLE);
                    if (!key) goto document_error;
                    value = ygp_document_add_scalar(&output_document, NULL,
                            input_event.data.mapping_start.anchor, -1,
                            YGP_DOUBLE_QUOTED_SCALAR_STYLE);
                    if (!value) goto document_error;
                    if (!ygp_document_append_mapping_pair(&output_document,
                                properties, key, value)) goto document_error;
                }

                /* Add 'tag': <tag>. */

                if (input_event.data.mapping_start.tag)
                {
                    key = ygp_document_add_scalar(&output_document, NULL,
                        "tag", -1, YGP_PLAIN_SCALAR_STYLE);
                    if (!key) goto document_error;
                    value = ygp_document_add_scalar(&output_document, NULL,
                            input_event.data.mapping_start.tag, -1,
                            YGP_DOUBLE_QUOTED_SCALAR_STYLE);
                    if (!value) goto document_error;
                    if (!ygp_document_append_mapping_pair(&output_document,
                                properties, key, value)) goto document_error;
                }

                /* Add 'implicit': <flag>. */

                key = ygp_document_add_scalar(&output_document, NULL,
                    "implicit", -1, YGP_PLAIN_SCALAR_STYLE);
                if (!key) goto document_error;
                value = ygp_document_add_scalar(&output_document, YGP_BOOL_TAG,
                        (input_event.data.mapping_start.implicit ?
                         "true" : "false"), -1, YGP_PLAIN_SCALAR_STYLE);
                if (!value) goto document_error;
                if (!ygp_document_append_mapping_pair(&output_document,
                            properties, key, value)) goto document_error;

                /* Display the style information. */

                if (input_event.data.sequence_start.style)
                {
                    ygp_sequence_style_t style
                        = input_event.data.mapping_start.style;

                    /* Add 'style': <style>. */

                    key = ygp_document_add_scalar(&output_document, NULL,
                        "style", -1, YGP_PLAIN_SCALAR_STYLE);
                    if (!key) goto document_error;
                    value = ygp_document_add_scalar(&output_document, NULL,
                            (style == YGP_BLOCK_MAPPING_STYLE ? "block" :
                             style == YGP_FLOW_MAPPING_STYLE ? "flow" :
                             "unknown"), -1, YGP_PLAIN_SCALAR_STYLE);
                    if (!value) goto document_error;
                    if (!ygp_document_append_mapping_pair(&output_document,
                                properties, key, value)) goto document_error;
                }

                break;

            case YGP_MAPPING_END_EVENT:

                /* Add 'type': 'MAPPING-END'. */

                key = ygp_document_add_scalar(&output_document, NULL,
                    "type", -1, YGP_PLAIN_SCALAR_STYLE);
                if (!key) goto document_error;
                value = ygp_document_add_scalar(&output_document, NULL,
                    "MAPPING-END", -1, YGP_PLAIN_SCALAR_STYLE);
                if (!value) goto document_error;
                if (!ygp_document_append_mapping_pair(&output_document,
                            properties, key, value)) goto document_error;

                break;

            default:
                /* It couldn't really happen. */
                break;
        }

        /* Delete the event object. */

        ygp_event_delete(&input_event);
    }

    if (!ygp_emitter_dump(&emitter, &output_document))
        goto emitter_error;
    if (!ygp_emitter_close(&emitter))
        goto emitter_error;

    ygp_parser_delete(&parser);
    ygp_emitter_delete(&emitter);

    return 0;

parser_error:

    /* Display a parser error message. */

    switch (parser.error)
    {
        case YGP_MEMORY_ERROR:
            fprintf(stderr, "Memory error: Not enough memory for parsing\n");
            break;

        case YGP_READER_ERROR:
            if (parser.problem_value != -1) {
                fprintf(stderr, "Reader error: %s: #%X at %d\n", parser.problem,
                        parser.problem_value, parser.problem_offset);
            }
            else {
                fprintf(stderr, "Reader error: %s at %d\n", parser.problem,
                        parser.problem_offset);
            }
            break;

        case YGP_SCANNER_ERROR:
            if (parser.context) {
                fprintf(stderr, "Scanner error: %s at line %d, column %d\n"
                        "%s at line %d, column %d\n", parser.context,
                        parser.context_mark.line+1, parser.context_mark.column+1,
                        parser.problem, parser.problem_mark.line+1,
                        parser.problem_mark.column+1);
            }
            else {
                fprintf(stderr, "Scanner error: %s at line %d, column %d\n",
                        parser.problem, parser.problem_mark.line+1,
                        parser.problem_mark.column+1);
            }
            break;

        case YGP_PARSER_ERROR:
            if (parser.context) {
                fprintf(stderr, "Parser error: %s at line %d, column %d\n"
                        "%s at line %d, column %d\n", parser.context,
                        parser.context_mark.line+1, parser.context_mark.column+1,
                        parser.problem, parser.problem_mark.line+1,
                        parser.problem_mark.column+1);
            }
            else {
                fprintf(stderr, "Parser error: %s at line %d, column %d\n",
                        parser.problem, parser.problem_mark.line+1,
                        parser.problem_mark.column+1);
            }
            break;

        default:
            /* Couldn't happen. */
            fprintf(stderr, "Internal error\n");
            break;
    }

    ygp_event_delete(&input_event);
    ygp_document_delete(&output_document);
    ygp_parser_delete(&parser);
    ygp_emitter_delete(&emitter);

    return 1;

emitter_error:

    /* Display an emitter error message. */

    switch (emitter.error)
    {
        case YGP_MEMORY_ERROR:
            fprintf(stderr, "Memory error: Not enough memory for emitting\n");
            break;

        case YGP_WRITER_ERROR:
            fprintf(stderr, "Writer error: %s\n", emitter.problem);
            break;

        case YGP_EMITTER_ERROR:
            fprintf(stderr, "Emitter error: %s\n", emitter.problem);
            break;

        default:
            /* Couldn't happen. */
            fprintf(stderr, "Internal error\n");
            break;
    }

    ygp_event_delete(&input_event);
    ygp_document_delete(&output_document);
    ygp_parser_delete(&parser);
    ygp_emitter_delete(&emitter);

    return 1;

document_error:

    fprintf(stderr, "Memory error: Not enough memory for creating a document\n");

    ygp_event_delete(&input_event);
    ygp_document_delete(&output_document);
    ygp_parser_delete(&parser);
    ygp_emitter_delete(&emitter);

    return 1;
}

