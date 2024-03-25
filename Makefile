###########
# Testing #
###########
test:
	@poetry run pytest

test-debug:
	@ # poetry run pytest -rF -l --log-level DEBUG
	poetry run pytest --log-level DEBUG

update-test:
	@cd tests && poetry run python update_test_02.py && cd
