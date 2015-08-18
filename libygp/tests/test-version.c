#include <ygp.h>

#include <stdlib.h>
#include <stdio.h>

#ifdef NDEBUG
#undef NDEBUG
#endif
#include <assert.h>

int
main(void)
{
    int major = -1;
    int minor = -1;
    int patch = -1;
    char buf[64];

    ygp_get_version(&major, &minor, &patch);
    sprintf(buf, "%d.%d.%d", major, minor, patch);
    assert(strcmp(buf, ygp_get_version_string()) == 0);

    /* Print structure sizes. */
    printf("sizeof(token) = %d\n", sizeof(ygp_token_t));
    printf("sizeof(event) = %d\n", sizeof(ygp_event_t));
    printf("sizeof(parser) = %d\n", sizeof(ygp_parser_t));

    return 0;
}
