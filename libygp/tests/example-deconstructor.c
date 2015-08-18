
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
    ygp_event_t output_event;

    /* Clear the objects. */

    memset(&parser, 0, sizeof(parser));
    memset(&emitter, 0, sizeof(emitter));
    memset(&input_event, 0, sizeof(input_event));
    memset(&output_event, 0, sizeof(output_event));

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

    if (!ygp_stream_start_event_initialize(&output_event, YGP_UTF8_ENCODING))
        goto event_error;
    if (!ygp_emitter_emit(&emitter, &output_event))
        goto emitter_error;

    /* Create and emit the DOCUMENT-START event. */

    if (!ygp_document_start_event_initialize(&output_event,
                NULL, NULL, NULL, 0))
        goto event_error;
    if (!ygp_emitter_emit(&emitter, &output_event))
        goto emitter_error;

    /* Create and emit the SEQUENCE-START event. */

    if (!ygp_sequence_start_event_initialize(&output_event,
                NULL, "tag:yaml.org,2002:seq", 1,
                YGP_BLOCK_SEQUENCE_STYLE))
        goto event_error;
    if (!ygp_emitter_emit(&emitter, &output_event))
        goto emitter_error;

    /* Loop through the input events. */

    while (!done)
    {
        /* Get the next event. */

        if (!ygp_parser_parse(&parser, &input_event))
            goto parser_error;

        /* Check if this is the stream end. */

        if (input_event.type == YGP_STREAM_END_EVENT) {
            done = 1;
        }

        /* Create and emit a MAPPING-START event. */

        if (!ygp_mapping_start_event_initialize(&output_event,
                    NULL, "tag:yaml.org,2002:map", 1,
                    YGP_BLOCK_MAPPING_STYLE))
            goto event_error;
        if (!ygp_emitter_emit(&emitter, &output_event))
            goto emitter_error;

        /* Analyze the event. */

        switch (input_event.type)
        {
            case YGP_STREAM_START_EVENT:

                /* Write 'type'. */

                if (!ygp_scalar_event_initialize(&output_event,
                            NULL, "tag:yaml.org,2002:str", "type", -1,
                            1, 1, YGP_PLAIN_SCALAR_STYLE))
                    goto event_error;
                if (!ygp_emitter_emit(&emitter, &output_event))
                    goto emitter_error;

                /* Write 'STREAM-START'. */

                if (!ygp_scalar_event_initialize(&output_event,
                            NULL, "tag:yaml.org,2002:str", "STREAM-START", -1,
                            1, 1, YGP_PLAIN_SCALAR_STYLE))
                    goto event_error;
                if (!ygp_emitter_emit(&emitter, &output_event))
                    goto emitter_error;

                /* Display encoding information. */

                if (input_event.data.stream_start.encoding)
                {
                    ygp_encoding_t encoding
                        = input_event.data.stream_start.encoding;

                    /* Write 'encoding'. */

                    if (!ygp_scalar_event_initialize(&output_event,
                                NULL, "tag:yaml.org,2002:str", "encoding", -1,
                                1, 1, YGP_PLAIN_SCALAR_STYLE))
                        goto event_error;
                    if (!ygp_emitter_emit(&emitter, &output_event))
                        goto emitter_error;

                    /* Write the stream encoding. */

                    if (!ygp_scalar_event_initialize(&output_event,
                                NULL, "tag:yaml.org,2002:str",
                                (encoding == YGP_UTF8_ENCODING ? "utf-8" :
                                 encoding == YGP_UTF16LE_ENCODING ? "utf-16-le" :
                                 encoding == YGP_UTF16BE_ENCODING ? "utf-16-be" :
                                 "unknown"), -1,
                                1, 1, YGP_PLAIN_SCALAR_STYLE))
                        goto event_error;
                    if (!ygp_emitter_emit(&emitter, &output_event))
                        goto emitter_error;
                }

                break;

            case YGP_STREAM_END_EVENT:

                /* Write 'type'. */

                if (!ygp_scalar_event_initialize(&output_event,
                            NULL, "tag:yaml.org,2002:str", "type", -1,
                            1, 1, YGP_PLAIN_SCALAR_STYLE))
                    goto event_error;
                if (!ygp_emitter_emit(&emitter, &output_event))
                    goto emitter_error;

                /* Write 'STREAM-END'. */

                if (!ygp_scalar_event_initialize(&output_event,
                            NULL, "tag:yaml.org,2002:str", "STREAM-END", -1,
                            1, 1, YGP_PLAIN_SCALAR_STYLE))
                    goto event_error;
                if (!ygp_emitter_emit(&emitter, &output_event))
                    goto emitter_error;

                break;

            case YGP_DOCUMENT_START_EVENT:

                /* Write 'type'. */

                if (!ygp_scalar_event_initialize(&output_event,
                            NULL, "tag:yaml.org,2002:str", "type", -1,
                            1, 1, YGP_PLAIN_SCALAR_STYLE))
                    goto event_error;
                if (!ygp_emitter_emit(&emitter, &output_event))
                    goto emitter_error;

                /* Write 'DOCUMENT-START'. */

                if (!ygp_scalar_event_initialize(&output_event,
                            NULL, "tag:yaml.org,2002:str", "DOCUMENT-START", -1,
                            1, 1, YGP_PLAIN_SCALAR_STYLE))
                    goto event_error;
                if (!ygp_emitter_emit(&emitter, &output_event))
                    goto emitter_error;

                /* Display the document version numbers. */

                if (input_event.data.document_start.version_directive)
                {
                    ygp_version_directive_t *version
                        = input_event.data.document_start.version_directive;
                    char number[64];

                    /* Write 'version'. */
                    
                    if (!ygp_scalar_event_initialize(&output_event,
                                NULL, "tag:yaml.org,2002:str", "version", -1,
                                1, 1, YGP_PLAIN_SCALAR_STYLE))
                        goto event_error;
                    if (!ygp_emitter_emit(&emitter, &output_event))
                        goto emitter_error;

                    /* Write '{'. */

                    if (!ygp_mapping_start_event_initialize(&output_event,
                                NULL, "tag:yaml.org,2002:map", 1,
                                YGP_FLOW_MAPPING_STYLE))
                        goto event_error;
                    if (!ygp_emitter_emit(&emitter, &output_event))
                        goto emitter_error;

                    /* Write 'major'. */

                    if (!ygp_scalar_event_initialize(&output_event,
                                NULL, "tag:yaml.org,2002:str", "major", -1,
                                1, 1, YGP_PLAIN_SCALAR_STYLE))
                        goto event_error;
                    if (!ygp_emitter_emit(&emitter, &output_event))
                        goto emitter_error;

                    /* Write a number. */

                    sprintf(number, "%d", version->major);
                    if (!ygp_scalar_event_initialize(&output_event,
                                NULL, "tag:yaml.org,2002:int", number, -1,
                                1, 1, YGP_PLAIN_SCALAR_STYLE))
                        goto event_error;
                    if (!ygp_emitter_emit(&emitter, &output_event))
                        goto emitter_error;

                    /* Write 'minor'. */

                    if (!ygp_scalar_event_initialize(&output_event,
                                NULL, "tag:yaml.org,2002:str", "minor", -1,
                                1, 1, YGP_PLAIN_SCALAR_STYLE))
                        goto event_error;
                    if (!ygp_emitter_emit(&emitter, &output_event))
                        goto emitter_error;

                    /* Write a number. */

                    sprintf(number, "%d", version->minor);
                    if (!ygp_scalar_event_initialize(&output_event,
                                NULL, "tag:yaml.org,2002:int", number, -1,
                                1, 1, YGP_PLAIN_SCALAR_STYLE))
                        goto event_error;
                    if (!ygp_emitter_emit(&emitter, &output_event))
                        goto emitter_error;

                    /* Write '}'. */

                    if (!ygp_mapping_end_event_initialize(&output_event))
                        goto event_error;
                    if (!ygp_emitter_emit(&emitter, &output_event))
                        goto emitter_error;
                }

                /* Display the document tag directives. */

                if (input_event.data.document_start.tag_directives.start
                        != input_event.data.document_start.tag_directives.end)
                {
                    ygp_tag_directive_t *tag;

                    /* Write 'tags'. */
                    
                    if (!ygp_scalar_event_initialize(&output_event,
                                NULL, "tag:yaml.org,2002:str", "tags", -1,
                                1, 1, YGP_PLAIN_SCALAR_STYLE))
                        goto event_error;
                    if (!ygp_emitter_emit(&emitter, &output_event))
                        goto emitter_error;

                    /* Start a block sequence. */

                    if (!ygp_sequence_start_event_initialize(&output_event,
                                NULL, "tag:yaml.org,2002:seq", 1,
                                YGP_BLOCK_SEQUENCE_STYLE))
                        goto event_error;
                    if (!ygp_emitter_emit(&emitter, &output_event))
                        goto emitter_error;

                    for (tag = input_event.data.document_start.tag_directives.start;
                            tag != input_event.data.document_start.tag_directives.end;
                            tag ++)
                    {
                        /* Write '{'. */

                        if (!ygp_mapping_start_event_initialize(&output_event,
                                    NULL, "tag:yaml.org,2002:map", 1,
                                    YGP_FLOW_MAPPING_STYLE))
                            goto event_error;
                        if (!ygp_emitter_emit(&emitter, &output_event))
                            goto emitter_error;

                        /* Write 'handle'. */

                        if (!ygp_scalar_event_initialize(&output_event,
                                    NULL, "tag:yaml.org,2002:str", "handle", -1,
                                    1, 1, YGP_PLAIN_SCALAR_STYLE))
                            goto event_error;
                        if (!ygp_emitter_emit(&emitter, &output_event))
                            goto emitter_error;

                        /* Write the tag directive handle. */

                        if (!ygp_scalar_event_initialize(&output_event,
                                    NULL, "tag:yaml.org,2002:str",
                                    tag->handle, -1,
                                    0, 1, YGP_DOUBLE_QUOTED_SCALAR_STYLE))
                            goto event_error;
                        if (!ygp_emitter_emit(&emitter, &output_event))
                            goto emitter_error;

                        /* Write 'prefix'. */

                        if (!ygp_scalar_event_initialize(&output_event,
                                    NULL, "tag:yaml.org,2002:str", "prefix", -1,
                                    1, 1, YGP_PLAIN_SCALAR_STYLE))
                            goto event_error;
                        if (!ygp_emitter_emit(&emitter, &output_event))
                            goto emitter_error;

                        /* Write the tag directive prefix. */

                        if (!ygp_scalar_event_initialize(&output_event,
                                    NULL, "tag:yaml.org,2002:str",
                                    tag->prefix, -1,
                                    0, 1, YGP_DOUBLE_QUOTED_SCALAR_STYLE))
                            goto event_error;
                        if (!ygp_emitter_emit(&emitter, &output_event))
                            goto emitter_error;

                        /* Write '}'. */

                        if (!ygp_mapping_end_event_initialize(&output_event))
                            goto event_error;
                        if (!ygp_emitter_emit(&emitter, &output_event))
                            goto emitter_error;
                    }

                    /* End a block sequence. */

                    if (!ygp_sequence_end_event_initialize(&output_event))
                        goto event_error;
                    if (!ygp_emitter_emit(&emitter, &output_event))
                        goto emitter_error;
                }

                /* Write 'implicit'. */

                if (!ygp_scalar_event_initialize(&output_event,
                            NULL, "tag:yaml.org,2002:str", "implicit", -1,
                            1, 1, YGP_PLAIN_SCALAR_STYLE))
                    goto event_error;
                if (!ygp_emitter_emit(&emitter, &output_event))
                    goto emitter_error;

                /* Write if the document is implicit. */

                if (!ygp_scalar_event_initialize(&output_event,
                            NULL, "tag:yaml.org,2002:bool",
                            (input_event.data.document_start.implicit ?
                             "true" : "false"), -1,
                            1, 0, YGP_PLAIN_SCALAR_STYLE))
                    goto event_error;
                if (!ygp_emitter_emit(&emitter, &output_event))
                    goto emitter_error;

                break;

            case YGP_DOCUMENT_END_EVENT:

                /* Write 'type'. */

                if (!ygp_scalar_event_initialize(&output_event,
                            NULL, "tag:yaml.org,2002:str", "type", -1,
                            1, 1, YGP_PLAIN_SCALAR_STYLE))
                    goto event_error;
                if (!ygp_emitter_emit(&emitter, &output_event))
                    goto emitter_error;

                /* Write 'DOCUMENT-END'. */

                if (!ygp_scalar_event_initialize(&output_event,
                            NULL, "tag:yaml.org,2002:str", "DOCUMENT-END", -1,
                            1, 1, YGP_PLAIN_SCALAR_STYLE))
                    goto event_error;
                if (!ygp_emitter_emit(&emitter, &output_event))
                    goto emitter_error;

                /* Write 'implicit'. */

                if (!ygp_scalar_event_initialize(&output_event,
                            NULL, "tag:yaml.org,2002:str", "implicit", -1,
                            1, 1, YGP_PLAIN_SCALAR_STYLE))
                    goto event_error;
                if (!ygp_emitter_emit(&emitter, &output_event))
                    goto emitter_error;

                /* Write if the document is implicit. */

                if (!ygp_scalar_event_initialize(&output_event,
                            NULL, "tag:yaml.org,2002:bool",
                            (input_event.data.document_end.implicit ?
                             "true" : "false"), -1,
                            1, 0, YGP_PLAIN_SCALAR_STYLE))
                    goto event_error;
                if (!ygp_emitter_emit(&emitter, &output_event))
                    goto emitter_error;

                break;

            case YGP_ALIAS_EVENT:

                /* Write 'type'. */

                if (!ygp_scalar_event_initialize(&output_event,
                            NULL, "tag:yaml.org,2002:str", "type", -1,
                            1, 1, YGP_PLAIN_SCALAR_STYLE))
                    goto event_error;
                if (!ygp_emitter_emit(&emitter, &output_event))
                    goto emitter_error;

                /* Write 'ALIAS'. */

                if (!ygp_scalar_event_initialize(&output_event,
                            NULL, "tag:yaml.org,2002:str", "ALIAS", -1,
                            1, 1, YGP_PLAIN_SCALAR_STYLE))
                    goto event_error;
                if (!ygp_emitter_emit(&emitter, &output_event))
                    goto emitter_error;

                /* Write 'anchor'. */

                if (!ygp_scalar_event_initialize(&output_event,
                            NULL, "tag:yaml.org,2002:str", "anchor", -1,
                            1, 1, YGP_PLAIN_SCALAR_STYLE))
                    goto event_error;
                if (!ygp_emitter_emit(&emitter, &output_event))
                    goto emitter_error;

                /* Write the alias anchor. */

                if (!ygp_scalar_event_initialize(&output_event,
                            NULL, "tag:yaml.org,2002:str",
                            input_event.data.alias.anchor, -1,
                            0, 1, YGP_DOUBLE_QUOTED_SCALAR_STYLE))
                    goto event_error;
                if (!ygp_emitter_emit(&emitter, &output_event))
                    goto emitter_error;

                break;

            case YGP_SCALAR_EVENT:

                /* Write 'type'. */

                if (!ygp_scalar_event_initialize(&output_event,
                            NULL, "tag:yaml.org,2002:str", "type", -1,
                            1, 1, YGP_PLAIN_SCALAR_STYLE))
                    goto event_error;
                if (!ygp_emitter_emit(&emitter, &output_event))
                    goto emitter_error;

                /* Write 'SCALAR'. */

                if (!ygp_scalar_event_initialize(&output_event,
                            NULL, "tag:yaml.org,2002:str", "SCALAR", -1,
                            1, 1, YGP_PLAIN_SCALAR_STYLE))
                    goto event_error;
                if (!ygp_emitter_emit(&emitter, &output_event))
                    goto emitter_error;

                /* Display the scalar anchor. */

                if (input_event.data.scalar.anchor)
                {
                    /* Write 'anchor'. */

                    if (!ygp_scalar_event_initialize(&output_event,
                                NULL, "tag:yaml.org,2002:str", "anchor", -1,
                                1, 1, YGP_PLAIN_SCALAR_STYLE))
                        goto event_error;
                    if (!ygp_emitter_emit(&emitter, &output_event))
                        goto emitter_error;

                    /* Write the scalar anchor. */

                    if (!ygp_scalar_event_initialize(&output_event,
                                NULL, "tag:yaml.org,2002:str",
                                input_event.data.scalar.anchor, -1,
                                0, 1, YGP_DOUBLE_QUOTED_SCALAR_STYLE))
                        goto event_error;
                    if (!ygp_emitter_emit(&emitter, &output_event))
                        goto emitter_error;
                }

                /* Display the scalar tag. */

                if (input_event.data.scalar.tag)
                {
                    /* Write 'tag'. */

                    if (!ygp_scalar_event_initialize(&output_event,
                                NULL, "tag:yaml.org,2002:str", "tag", -1,
                                1, 1, YGP_PLAIN_SCALAR_STYLE))
                        goto event_error;
                    if (!ygp_emitter_emit(&emitter, &output_event))
                        goto emitter_error;

                    /* Write the scalar tag. */

                    if (!ygp_scalar_event_initialize(&output_event,
                                NULL, "tag:yaml.org,2002:str",
                                input_event.data.scalar.tag, -1,
                                0, 1, YGP_DOUBLE_QUOTED_SCALAR_STYLE))
                        goto event_error;
                    if (!ygp_emitter_emit(&emitter, &output_event))
                        goto emitter_error;
                }

                /* Display the scalar value. */

                /* Write 'value'. */

                if (!ygp_scalar_event_initialize(&output_event,
                            NULL, "tag:yaml.org,2002:str", "value", -1,
                            1, 1, YGP_PLAIN_SCALAR_STYLE))
                    goto event_error;
                if (!ygp_emitter_emit(&emitter, &output_event))
                    goto emitter_error;

                /* Write the scalar value. */

                if (!ygp_scalar_event_initialize(&output_event,
                            NULL, "tag:yaml.org,2002:str",
                            input_event.data.scalar.value,
                            input_event.data.scalar.length,
                            0, 1, YGP_DOUBLE_QUOTED_SCALAR_STYLE))
                    goto event_error;
                if (!ygp_emitter_emit(&emitter, &output_event))
                    goto emitter_error;

                /* Display if the scalar tag is implicit. */

                /* Write 'implicit'. */
                
                if (!ygp_scalar_event_initialize(&output_event,
                            NULL, "tag:yaml.org,2002:str", "implicit", -1,
                            1, 1, YGP_PLAIN_SCALAR_STYLE))
                    goto event_error;
                if (!ygp_emitter_emit(&emitter, &output_event))
                    goto emitter_error;

                /* Write '{'. */

                if (!ygp_mapping_start_event_initialize(&output_event,
                            NULL, "tag:yaml.org,2002:map", 1,
                            YGP_FLOW_MAPPING_STYLE))
                    goto event_error;
                if (!ygp_emitter_emit(&emitter, &output_event))
                    goto emitter_error;

                /* Write 'plain'. */

                if (!ygp_scalar_event_initialize(&output_event,
                            NULL, "tag:yaml.org,2002:str", "plain", -1,
                            1, 1, YGP_PLAIN_SCALAR_STYLE))
                    goto event_error;
                if (!ygp_emitter_emit(&emitter, &output_event))
                    goto emitter_error;

                /* Write if the scalar is implicit in the plain style. */

                if (!ygp_scalar_event_initialize(&output_event,
                            NULL, "tag:yaml.org,2002:bool",
                            (input_event.data.scalar.plain_implicit ?
                             "true" : "false"), -1,
                            1, 0, YGP_PLAIN_SCALAR_STYLE))
                    goto event_error;
                if (!ygp_emitter_emit(&emitter, &output_event))
                    goto emitter_error;

                /* Write 'quoted'. */

                if (!ygp_scalar_event_initialize(&output_event,
                            NULL, "tag:yaml.org,2002:str", "non-plain", -1,
                            1, 1, YGP_PLAIN_SCALAR_STYLE))
                    goto event_error;
                if (!ygp_emitter_emit(&emitter, &output_event))
                    goto emitter_error;

                /* Write if the scalar is implicit in a non-plain style. */

                if (!ygp_scalar_event_initialize(&output_event,
                            NULL, "tag:yaml.org,2002:bool",
                            (input_event.data.scalar.quoted_implicit ?
                             "true" : "false"), -1,
                            1, 0, YGP_PLAIN_SCALAR_STYLE))
                    goto event_error;
                if (!ygp_emitter_emit(&emitter, &output_event))
                    goto emitter_error;

                /* Write '}'. */

                if (!ygp_mapping_end_event_initialize(&output_event))
                    goto event_error;
                if (!ygp_emitter_emit(&emitter, &output_event))
                    goto emitter_error;

                /* Display the style information. */

                if (input_event.data.scalar.style)
                {
                    ygp_scalar_style_t style = input_event.data.scalar.style;

                    /* Write 'style'. */

                    if (!ygp_scalar_event_initialize(&output_event,
                                NULL, "tag:yaml.org,2002:str", "style", -1,
                                1, 1, YGP_PLAIN_SCALAR_STYLE))
                        goto event_error;
                    if (!ygp_emitter_emit(&emitter, &output_event))
                        goto emitter_error;

                    /* Write the scalar style. */

                    if (!ygp_scalar_event_initialize(&output_event,
                                NULL, "tag:yaml.org,2002:str",
                                (style == YGP_PLAIN_SCALAR_STYLE ? "plain" :
                                 style == YGP_SINGLE_QUOTED_SCALAR_STYLE ?
                                        "single-quoted" :
                                 style == YGP_DOUBLE_QUOTED_SCALAR_STYLE ?
                                        "double-quoted" :
                                 style == YGP_LITERAL_SCALAR_STYLE ? "literal" :
                                 style == YGP_FOLDED_SCALAR_STYLE ? "folded" :
                                 "unknown"), -1,
                                1, 1, YGP_PLAIN_SCALAR_STYLE))
                        goto event_error;
                    if (!ygp_emitter_emit(&emitter, &output_event))
                        goto emitter_error;
                }

                break;

            case YGP_SEQUENCE_START_EVENT:

                /* Write 'type'. */

                if (!ygp_scalar_event_initialize(&output_event,
                            NULL, "tag:yaml.org,2002:str", "type", -1,
                            1, 1, YGP_PLAIN_SCALAR_STYLE))
                    goto event_error;
                if (!ygp_emitter_emit(&emitter, &output_event))
                    goto emitter_error;

                /* Write 'SEQUENCE-START'. */

                if (!ygp_scalar_event_initialize(&output_event,
                            NULL, "tag:yaml.org,2002:str", "SEQUENCE-START", -1,
                            1, 1, YGP_PLAIN_SCALAR_STYLE))
                    goto event_error;
                if (!ygp_emitter_emit(&emitter, &output_event))
                    goto emitter_error;

                /* Display the sequence anchor. */

                if (input_event.data.sequence_start.anchor)
                {
                    /* Write 'anchor'. */

                    if (!ygp_scalar_event_initialize(&output_event,
                                NULL, "tag:yaml.org,2002:str", "anchor", -1,
                                1, 1, YGP_PLAIN_SCALAR_STYLE))
                        goto event_error;
                    if (!ygp_emitter_emit(&emitter, &output_event))
                        goto emitter_error;

                    /* Write the sequence anchor. */

                    if (!ygp_scalar_event_initialize(&output_event,
                                NULL, "tag:yaml.org,2002:str",
                                input_event.data.sequence_start.anchor, -1,
                                0, 1, YGP_DOUBLE_QUOTED_SCALAR_STYLE))
                        goto event_error;
                    if (!ygp_emitter_emit(&emitter, &output_event))
                        goto emitter_error;
                }

                /* Display the sequence tag. */

                if (input_event.data.sequence_start.tag)
                {
                    /* Write 'tag'. */

                    if (!ygp_scalar_event_initialize(&output_event,
                                NULL, "tag:yaml.org,2002:str", "tag", -1,
                                1, 1, YGP_PLAIN_SCALAR_STYLE))
                        goto event_error;
                    if (!ygp_emitter_emit(&emitter, &output_event))
                        goto emitter_error;

                    /* Write the sequence tag. */

                    if (!ygp_scalar_event_initialize(&output_event,
                                NULL, "tag:yaml.org,2002:str",
                                input_event.data.sequence_start.tag, -1,
                                0, 1, YGP_DOUBLE_QUOTED_SCALAR_STYLE))
                        goto event_error;
                    if (!ygp_emitter_emit(&emitter, &output_event))
                        goto emitter_error;
                }

                /* Write 'implicit'. */

                if (!ygp_scalar_event_initialize(&output_event,
                            NULL, "tag:yaml.org,2002:str", "implicit", -1,
                            1, 1, YGP_PLAIN_SCALAR_STYLE))
                    goto event_error;
                if (!ygp_emitter_emit(&emitter, &output_event))
                    goto emitter_error;

                /* Write if the sequence tag is implicit. */

                if (!ygp_scalar_event_initialize(&output_event,
                            NULL, "tag:yaml.org,2002:bool",
                            (input_event.data.sequence_start.implicit ?
                             "true" : "false"), -1,
                            1, 0, YGP_PLAIN_SCALAR_STYLE))
                    goto event_error;
                if (!ygp_emitter_emit(&emitter, &output_event))
                    goto emitter_error;

                /* Display the style information. */

                if (input_event.data.sequence_start.style)
                {
                    ygp_sequence_style_t style
                        = input_event.data.sequence_start.style;

                    /* Write 'style'. */

                    if (!ygp_scalar_event_initialize(&output_event,
                                NULL, "tag:yaml.org,2002:str", "style", -1,
                                1, 1, YGP_PLAIN_SCALAR_STYLE))
                        goto event_error;
                    if (!ygp_emitter_emit(&emitter, &output_event))
                        goto emitter_error;

                    /* Write the scalar style. */

                    if (!ygp_scalar_event_initialize(&output_event,
                                NULL, "tag:yaml.org,2002:str",
                                (style == YGP_BLOCK_SEQUENCE_STYLE ? "block" :
                                 style == YGP_FLOW_SEQUENCE_STYLE ? "flow" :
                                 "unknown"), -1,
                                1, 1, YGP_PLAIN_SCALAR_STYLE))
                        goto event_error;
                    if (!ygp_emitter_emit(&emitter, &output_event))
                        goto emitter_error;
                }

                break;

            case YGP_SEQUENCE_END_EVENT:

                /* Write 'type'. */

                if (!ygp_scalar_event_initialize(&output_event,
                            NULL, "tag:yaml.org,2002:str", "type", -1,
                            1, 1, YGP_PLAIN_SCALAR_STYLE))
                    goto event_error;
                if (!ygp_emitter_emit(&emitter, &output_event))
                    goto emitter_error;

                /* Write 'SEQUENCE-END'. */

                if (!ygp_scalar_event_initialize(&output_event,
                            NULL, "tag:yaml.org,2002:str", "SEQUENCE-END", -1,
                            1, 1, YGP_PLAIN_SCALAR_STYLE))
                    goto event_error;
                if (!ygp_emitter_emit(&emitter, &output_event))
                    goto emitter_error;

                break;

            case YGP_MAPPING_START_EVENT:

                /* Write 'type'. */

                if (!ygp_scalar_event_initialize(&output_event,
                            NULL, "tag:yaml.org,2002:str", "type", -1,
                            1, 1, YGP_PLAIN_SCALAR_STYLE))
                    goto event_error;
                if (!ygp_emitter_emit(&emitter, &output_event))
                    goto emitter_error;

                /* Write 'MAPPING-START'. */

                if (!ygp_scalar_event_initialize(&output_event,
                            NULL, "tag:yaml.org,2002:str", "MAPPING-START", -1,
                            1, 1, YGP_PLAIN_SCALAR_STYLE))
                    goto event_error;
                if (!ygp_emitter_emit(&emitter, &output_event))
                    goto emitter_error;

                /* Display the mapping anchor. */

                if (input_event.data.mapping_start.anchor)
                {
                    /* Write 'anchor'. */

                    if (!ygp_scalar_event_initialize(&output_event,
                                NULL, "tag:yaml.org,2002:str", "anchor", -1,
                                1, 1, YGP_PLAIN_SCALAR_STYLE))
                        goto event_error;
                    if (!ygp_emitter_emit(&emitter, &output_event))
                        goto emitter_error;

                    /* Write the mapping anchor. */

                    if (!ygp_scalar_event_initialize(&output_event,
                                NULL, "tag:yaml.org,2002:str",
                                input_event.data.mapping_start.anchor, -1,
                                0, 1, YGP_DOUBLE_QUOTED_SCALAR_STYLE))
                        goto event_error;
                    if (!ygp_emitter_emit(&emitter, &output_event))
                        goto emitter_error;
                }

                /* Display the mapping tag. */

                if (input_event.data.mapping_start.tag)
                {
                    /* Write 'tag'. */

                    if (!ygp_scalar_event_initialize(&output_event,
                                NULL, "tag:yaml.org,2002:str", "tag", -1,
                                1, 1, YGP_PLAIN_SCALAR_STYLE))
                        goto event_error;
                    if (!ygp_emitter_emit(&emitter, &output_event))
                        goto emitter_error;

                    /* Write the mapping tag. */

                    if (!ygp_scalar_event_initialize(&output_event,
                                NULL, "tag:yaml.org,2002:str",
                                input_event.data.mapping_start.tag, -1,
                                0, 1, YGP_DOUBLE_QUOTED_SCALAR_STYLE))
                        goto event_error;
                    if (!ygp_emitter_emit(&emitter, &output_event))
                        goto emitter_error;
                }

                /* Write 'implicit'. */

                if (!ygp_scalar_event_initialize(&output_event,
                            NULL, "tag:yaml.org,2002:str", "implicit", -1,
                            1, 1, YGP_PLAIN_SCALAR_STYLE))
                    goto event_error;
                if (!ygp_emitter_emit(&emitter, &output_event))
                    goto emitter_error;

                /* Write if the mapping tag is implicit. */

                if (!ygp_scalar_event_initialize(&output_event,
                            NULL, "tag:yaml.org,2002:bool",
                            (input_event.data.mapping_start.implicit ?
                             "true" : "false"), -1,
                            1, 0, YGP_PLAIN_SCALAR_STYLE))
                    goto event_error;
                if (!ygp_emitter_emit(&emitter, &output_event))
                    goto emitter_error;

                /* Display the style information. */

                if (input_event.data.mapping_start.style)
                {
                    ygp_mapping_style_t style
                        = input_event.data.mapping_start.style;

                    /* Write 'style'. */

                    if (!ygp_scalar_event_initialize(&output_event,
                                NULL, "tag:yaml.org,2002:str", "style", -1,
                                1, 1, YGP_PLAIN_SCALAR_STYLE))
                        goto event_error;
                    if (!ygp_emitter_emit(&emitter, &output_event))
                        goto emitter_error;

                    /* Write the scalar style. */

                    if (!ygp_scalar_event_initialize(&output_event,
                                NULL, "tag:yaml.org,2002:str",
                                (style == YGP_BLOCK_MAPPING_STYLE ? "block" :
                                 style == YGP_FLOW_MAPPING_STYLE ? "flow" :
                                 "unknown"), -1,
                                1, 1, YGP_PLAIN_SCALAR_STYLE))
                        goto event_error;
                    if (!ygp_emitter_emit(&emitter, &output_event))
                        goto emitter_error;
                }

                break;

            case YGP_MAPPING_END_EVENT:

                /* Write 'type'. */

                if (!ygp_scalar_event_initialize(&output_event,
                            NULL, "tag:yaml.org,2002:str", "type", -1,
                            1, 1, YGP_PLAIN_SCALAR_STYLE))
                    goto event_error;
                if (!ygp_emitter_emit(&emitter, &output_event))
                    goto emitter_error;

                /* Write 'MAPPING-END'. */

                if (!ygp_scalar_event_initialize(&output_event,
                            NULL, "tag:yaml.org,2002:str", "MAPPING-END", -1,
                            1, 1, YGP_PLAIN_SCALAR_STYLE))
                    goto event_error;
                if (!ygp_emitter_emit(&emitter, &output_event))
                    goto emitter_error;

                break;

            default:
                /* It couldn't really happen. */
                break;
        }

        /* Delete the event object. */

        ygp_event_delete(&input_event);

        /* Create and emit a MAPPING-END event. */

        if (!ygp_mapping_end_event_initialize(&output_event))
            goto event_error;
        if (!ygp_emitter_emit(&emitter, &output_event))
            goto emitter_error;
    }

    /* Create and emit the SEQUENCE-END event. */

    if (!ygp_sequence_end_event_initialize(&output_event))
        goto event_error;
    if (!ygp_emitter_emit(&emitter, &output_event))
        goto emitter_error;

    /* Create and emit the DOCUMENT-END event. */

    if (!ygp_document_end_event_initialize(&output_event, 0))
        goto event_error;
    if (!ygp_emitter_emit(&emitter, &output_event))
        goto emitter_error;

    /* Create and emit the STREAM-END event. */

    if (!ygp_stream_end_event_initialize(&output_event))
        goto event_error;
    if (!ygp_emitter_emit(&emitter, &output_event))
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
    ygp_event_delete(&output_event);
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
    ygp_event_delete(&output_event);
    ygp_parser_delete(&parser);
    ygp_emitter_delete(&emitter);

    return 1;

event_error:

    fprintf(stderr, "Memory error: Not enough memory for creating an event\n");

    ygp_event_delete(&input_event);
    ygp_event_delete(&output_event);
    ygp_parser_delete(&parser);
    ygp_emitter_delete(&emitter);

    return 1;
}

