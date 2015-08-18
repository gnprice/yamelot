AM_CPPFLAGS = -I$(top_srcdir)/include
lib_LTLIBRARIES = libygp.la
libygp_la_SOURCES = ygp_private.h api.c reader.c scanner.c parser.c loader.c writer.c emitter.c dumper.c
libygp_la_LDFLAGS = -release $(YGP_LT_RELEASE) -version-info $(YGP_LT_CURRENT):$(YGP_LT_REVISION):$(YGP_LT_AGE)
