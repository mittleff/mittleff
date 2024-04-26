SUBDIRS = libpyffi pyffi
dist_data_DATA = pyffi.scm

GUILE=LD_LIBRARY_PATH=libpyffi/.libs guile -L .
TEST_EXTENSIONS = .scm
TESTS_ENVIRONMENT = GUILE_AUTO_COMPILE=0
SCM_LOG_DRIVER = $(GUILE) $(srcdir)/build-aux/test-driver.scm

TESTS = test-pyffi.scm
