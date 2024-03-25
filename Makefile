###########
# Testing #
###########
.PHONY: test test-debug test-update
test:
	@poetry run pytest

test-debug:
	@ # poetry run pytest -rF -l --log-level DEBUG
	poetry run pytest --log-level DEBUG

test-update:
	@poetry run python -c 'import mittleff.testing; mittleff.testing.update_test_02_mittleff()'
	@mv tests/test_02_mittleff.py tests/$(date date +%Y-%m-%d_%H-%M-%S)-test_02_mittleff.py 
	@mv test_02_mittleff.py tests/
