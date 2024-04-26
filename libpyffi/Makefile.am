lib_LTLIBRARIES = libpyffi.la
libpyffi_la_SOURCES = pyffi.c
libpyffi_la_CFLAGS = -shared -fPIC $(GUILE_CFLAGS) `python$(PYTHON_VERSION)-config --cflags` `python$(PYTHON_VERSION)-config --ldflags` -lpython$(PYTHON_VERSION)
libpyffi_la_LDFLAGS = -version-info 0:0:0
