#include <ygp.h>

#include <stdlib.h>
#include <stdio.h>

#ifdef NDEBUG
#undef NDEBUG
#endif
#include <assert.h>

int
main(int argc, char *argv[])
{
    int number;

    if (argc < 2) {
        printf("Usage: %s file1.yaml ...\n", argv[0]);
        return 0;
    }

    for (number = 1; number < argc; number ++)
    {
        FILE *file;
        ygp_parser_t parser;
        ygp_document_t document;
        int done = 0;
        int count = 0;
        int error = 0;

        printf("[%d] Loading '%s': ", number, argv[number]);
        fflush(stdout);

        file = fopen(argv[number], "rb");
        assert(file);

        assert(ygp_parser_initialize(&parser));

        ygp_parser_set_input_file(&parser, file);

        while (!done)
        {
            if (!ygp_parser_load(&parser, &document)) {
                error = 1;
                break;
            }

            done = (!ygp_document_get_root_node(&document));

            ygp_document_delete(&document);

            if (!done) count ++;
        }

        ygp_parser_delete(&parser);

        assert(!fclose(file));

        printf("%s (%d documents)\n", (error ? "FAILURE" : "SUCCESS"), count);
    }

    return 0;
}

