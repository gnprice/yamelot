AM_CPPFLAGS = -I$(top_srcdir)/include
#AM_CFLAGS = -Wno-pointer-sign
LDADD = $(top_builddir)/src/libygp.la
TESTS = test-version test-reader
check_PROGRAMS = test-version test-reader
noinst_PROGRAMS = run-scanner run-parser run-loader run-emitter run-dumper	\
				  example-reformatter example-reformatter-alt	\
				  example-deconstructor example-deconstructor-alt
