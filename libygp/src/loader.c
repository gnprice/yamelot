
#include "ygp_private.h"

/*
 * API functions.
 */

YGP_DECLARE(int)
ygp_parser_load(ygp_parser_t *parser, ygp_document_t *document);

/*
 * Error handling.
 */

static int
ygp_parser_set_composer_error(ygp_parser_t *parser,
        const char *problem, ygp_mark_t problem_mark);

static int
ygp_parser_set_composer_error_context(ygp_parser_t *parser,
        const char *context, ygp_mark_t context_mark,
        const char *problem, ygp_mark_t problem_mark);


/*
 * Alias handling.
 */

static int
ygp_parser_register_anchor(ygp_parser_t *parser,
        int index, ygp_char_t *anchor);

/*
 * Clean up functions.
 */

static void
ygp_parser_delete_aliases(ygp_parser_t *parser);

/*
 * Composer functions.
 */

static int
ygp_parser_load_document(ygp_parser_t *parser, ygp_event_t *first_event);

static int
ygp_parser_load_node(ygp_parser_t *parser, ygp_event_t *first_event);

static int
ygp_parser_load_alias(ygp_parser_t *parser, ygp_event_t *first_event);

static int
ygp_parser_load_scalar(ygp_parser_t *parser, ygp_event_t *first_event);

static int
ygp_parser_load_sequence(ygp_parser_t *parser, ygp_event_t *first_event);

static int
ygp_parser_load_mapping(ygp_parser_t *parser, ygp_event_t *first_event);

/*
 * Load the next document of the stream.
 */

YGP_DECLARE(int)
ygp_parser_load(ygp_parser_t *parser, ygp_document_t *document)
{
    ygp_event_t event;

    assert(parser);     /* Non-NULL parser object is expected. */
    assert(document);   /* Non-NULL document object is expected. */

    memset(document, 0, sizeof(ygp_document_t));
    if (!STACK_INIT(parser, document->nodes, INITIAL_STACK_SIZE))
        goto error;

    if (!parser->stream_start_produced) {
        if (!ygp_parser_parse(parser, &event)) goto error;
        assert(event.type == YGP_STREAM_START_EVENT);
                        /* STREAM-START is expected. */
    }

    if (parser->stream_end_produced) {
        return 1;
    }

    if (!ygp_parser_parse(parser, &event)) goto error;
    if (event.type == YGP_STREAM_END_EVENT) {
        return 1;
    }

    if (!STACK_INIT(parser, parser->aliases, INITIAL_STACK_SIZE))
        goto error;

    parser->document = document;

    if (!ygp_parser_load_document(parser, &event)) goto error;

    ygp_parser_delete_aliases(parser);
    parser->document = NULL;

    return 1;

error:

    ygp_parser_delete_aliases(parser);
    ygp_document_delete(document);
    parser->document = NULL;

    return 0;
}

/*
 * Set composer error.
 */

static int
ygp_parser_set_composer_error(ygp_parser_t *parser,
        const char *problem, ygp_mark_t problem_mark)
{
    parser->error = YGP_COMPOSER_ERROR;
    parser->problem = problem;
    parser->problem_mark = problem_mark;

    return 0;
}

/*
 * Set composer error with context.
 */

static int
ygp_parser_set_composer_error_context(ygp_parser_t *parser,
        const char *context, ygp_mark_t context_mark,
        const char *problem, ygp_mark_t problem_mark)
{
    parser->error = YGP_COMPOSER_ERROR;
    parser->context = context;
    parser->context_mark = context_mark;
    parser->problem = problem;
    parser->problem_mark = problem_mark;

    return 0;
}

/*
 * Delete the stack of aliases.
 */

static void
ygp_parser_delete_aliases(ygp_parser_t *parser)
{
    while (!STACK_EMPTY(parser, parser->aliases)) {
        ygp_free(POP(parser, parser->aliases).anchor);
    }
    STACK_DEL(parser, parser->aliases);
}

/*
 * Compose a document object.
 */

static int
ygp_parser_load_document(ygp_parser_t *parser, ygp_event_t *first_event)
{
    ygp_event_t event;

    assert(first_event->type == YGP_DOCUMENT_START_EVENT);
                        /* DOCUMENT-START is expected. */

    parser->document->version_directive
        = first_event->data.document_start.version_directive;
    parser->document->tag_directives.start
        = first_event->data.document_start.tag_directives.start;
    parser->document->tag_directives.end
        = first_event->data.document_start.tag_directives.end;
    parser->document->start_implicit
        = first_event->data.document_start.implicit;
    parser->document->start_mark = first_event->start_mark;

    if (!ygp_parser_parse(parser, &event)) return 0;

    if (!ygp_parser_load_node(parser, &event)) return 0;

    if (!ygp_parser_parse(parser, &event)) return 0;
    assert(event.type == YGP_DOCUMENT_END_EVENT);
                        /* DOCUMENT-END is expected. */

    parser->document->end_implicit = event.data.document_end.implicit;
    parser->document->end_mark = event.end_mark;

    return 1;
}

/*
 * Compose a node.
 */

static int
ygp_parser_load_node(ygp_parser_t *parser, ygp_event_t *first_event)
{
    switch (first_event->type) {
        case YGP_ALIAS_EVENT:
            return ygp_parser_load_alias(parser, first_event);
        case YGP_SCALAR_EVENT:
            return ygp_parser_load_scalar(parser, first_event);
        case YGP_SEQUENCE_START_EVENT:
            return ygp_parser_load_sequence(parser, first_event);
        case YGP_MAPPING_START_EVENT:
            return ygp_parser_load_mapping(parser, first_event);
        default:
            assert(0);  /* Could not happen. */
            return 0;
    }

    return 0;
}

/*
 * Add an anchor.
 */

static int
ygp_parser_register_anchor(ygp_parser_t *parser,
        int index, ygp_char_t *anchor)
{
    ygp_alias_data_t data;
    ygp_alias_data_t *alias_data;

    if (!anchor) return 1;

    data.anchor = anchor;
    data.index = index;
    data.mark = parser->document->nodes.start[index-1].start_mark;

    for (alias_data = parser->aliases.start;
            alias_data != parser->aliases.top; alias_data ++) {
        if (strcmp((char *)alias_data->anchor, (char *)anchor) == 0) {
            ygp_free(anchor);
            return ygp_parser_set_composer_error_context(parser,
                    "found duplicate anchor; first occurence",
                    alias_data->mark, "second occurence", data.mark);
        }
    }

    if (!PUSH(parser, parser->aliases, data)) {
        ygp_free(anchor);
        return 0;
    }

    return 1;
}

/*
 * Compose a node corresponding to an alias.
 */

static int
ygp_parser_load_alias(ygp_parser_t *parser, ygp_event_t *first_event)
{
    ygp_char_t *anchor = first_event->data.alias.anchor;
    ygp_alias_data_t *alias_data;

    for (alias_data = parser->aliases.start;
            alias_data != parser->aliases.top; alias_data ++) {
        if (strcmp((char *)alias_data->anchor, (char *)anchor) == 0) {
            ygp_free(anchor);
            return alias_data->index;
        }
    }

    ygp_free(anchor);
    return ygp_parser_set_composer_error(parser, "found undefined alias",
            first_event->start_mark);
}

/*
 * Compose a scalar node.
 */

static int
ygp_parser_load_scalar(ygp_parser_t *parser, ygp_event_t *first_event)
{
    ygp_node_t node;
    int index;
    ygp_char_t *tag = first_event->data.scalar.tag;

    if (!STACK_LIMIT(parser, parser->document->nodes, INT_MAX-1)) goto error;

    if (!tag || strcmp((char *)tag, "!") == 0) {
        ygp_free(tag);
        tag = ygp_strdup((ygp_char_t *)YGP_DEFAULT_SCALAR_TAG);
        if (!tag) goto error;
    }

    SCALAR_NODE_INIT(node, tag, first_event->data.scalar.value,
            first_event->data.scalar.length, first_event->data.scalar.style,
            first_event->start_mark, first_event->end_mark);

    if (!PUSH(parser, parser->document->nodes, node)) goto error;

    index = parser->document->nodes.top - parser->document->nodes.start;

    if (!ygp_parser_register_anchor(parser, index,
                first_event->data.scalar.anchor)) return 0;

    return index;

error:
    ygp_free(tag);
    ygp_free(first_event->data.scalar.anchor);
    ygp_free(first_event->data.scalar.value);
    return 0;
}

/*
 * Compose a sequence node.
 */

static int
ygp_parser_load_sequence(ygp_parser_t *parser, ygp_event_t *first_event)
{
    ygp_event_t event;
    ygp_node_t node;
    struct {
        ygp_node_item_t *start;
        ygp_node_item_t *end;
        ygp_node_item_t *top;
    } items = { NULL, NULL, NULL };
    int index, item_index;
    ygp_char_t *tag = first_event->data.sequence_start.tag;

    if (!STACK_LIMIT(parser, parser->document->nodes, INT_MAX-1)) goto error;

    if (!tag || strcmp((char *)tag, "!") == 0) {
        ygp_free(tag);
        tag = ygp_strdup((ygp_char_t *)YGP_DEFAULT_SEQUENCE_TAG);
        if (!tag) goto error;
    }

    if (!STACK_INIT(parser, items, INITIAL_STACK_SIZE)) goto error;

    SEQUENCE_NODE_INIT(node, tag, items.start, items.end,
            first_event->data.sequence_start.style,
            first_event->start_mark, first_event->end_mark);

    if (!PUSH(parser, parser->document->nodes, node)) goto error;

    index = parser->document->nodes.top - parser->document->nodes.start;

    if (!ygp_parser_register_anchor(parser, index,
                first_event->data.sequence_start.anchor)) return 0;

    if (!ygp_parser_parse(parser, &event)) return 0;

    while (event.type != YGP_SEQUENCE_END_EVENT) {
        if (!STACK_LIMIT(parser,
                    parser->document->nodes.start[index-1].data.sequence.items,
                    INT_MAX-1)) return 0;
        item_index = ygp_parser_load_node(parser, &event);
        if (!item_index) return 0;
        if (!PUSH(parser,
                    parser->document->nodes.start[index-1].data.sequence.items,
                    item_index)) return 0;
        if (!ygp_parser_parse(parser, &event)) return 0;
    }

    parser->document->nodes.start[index-1].end_mark = event.end_mark;

    return index;

error:
    ygp_free(tag);
    ygp_free(first_event->data.sequence_start.anchor);
    return 0;
}

/*
 * Compose a mapping node.
 */

static int
ygp_parser_load_mapping(ygp_parser_t *parser, ygp_event_t *first_event)
{
    ygp_event_t event;
    ygp_node_t node;
    struct {
        ygp_node_pair_t *start;
        ygp_node_pair_t *end;
        ygp_node_pair_t *top;
    } pairs = { NULL, NULL, NULL };
    int index;
    ygp_node_pair_t pair;
    ygp_char_t *tag = first_event->data.mapping_start.tag;

    if (!STACK_LIMIT(parser, parser->document->nodes, INT_MAX-1)) goto error;

    if (!tag || strcmp((char *)tag, "!") == 0) {
        ygp_free(tag);
        tag = ygp_strdup((ygp_char_t *)YGP_DEFAULT_MAPPING_TAG);
        if (!tag) goto error;
    }

    if (!STACK_INIT(parser, pairs, INITIAL_STACK_SIZE)) goto error;

    MAPPING_NODE_INIT(node, tag, pairs.start, pairs.end,
            first_event->data.mapping_start.style,
            first_event->start_mark, first_event->end_mark);

    if (!PUSH(parser, parser->document->nodes, node)) goto error;

    index = parser->document->nodes.top - parser->document->nodes.start;

    if (!ygp_parser_register_anchor(parser, index,
                first_event->data.mapping_start.anchor)) return 0;

    if (!ygp_parser_parse(parser, &event)) return 0;

    while (event.type != YGP_MAPPING_END_EVENT) {
        if (!STACK_LIMIT(parser,
                    parser->document->nodes.start[index-1].data.mapping.pairs,
                    INT_MAX-1)) return 0;
        pair.key = ygp_parser_load_node(parser, &event);
        if (!pair.key) return 0;
        if (!ygp_parser_parse(parser, &event)) return 0;
        pair.value = ygp_parser_load_node(parser, &event);
        if (!pair.value) return 0;
        if (!PUSH(parser,
                    parser->document->nodes.start[index-1].data.mapping.pairs,
                    pair)) return 0;
        if (!ygp_parser_parse(parser, &event)) return 0;
    }

    parser->document->nodes.start[index-1].end_mark = event.end_mark;

    return index;

error:
    ygp_free(tag);
    ygp_free(first_event->data.mapping_start.anchor);
    return 0;
}

