all: interface.scm
interface.scm: interface.in.scm
	sed 's/PYTHONSHAREDOBJ/"libpython$(PYTHON_VERSION)$(PYTHON_ABI)"/' interface.in.scm > interface.scm
clean-local: clean-local-check
.PHONY: clean-local-check
clean-local-check:
	rm interface.scm
