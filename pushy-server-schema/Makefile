TEST_DB = opscode_pushy_test

all : setup_schema setup_tests test

TEST_FUNCTIONS = $(wildcard t/test_*.sql)

setup_schema:
	@echo "Dropping and recreating database '$(TEST_DB)'"
	@psql --dbname template1 --single-transaction --command 'DROP DATABASE IF EXISTS $(TEST_DB)'
	@createdb $(TEST_DB)
	@sqitch --engine pg --db-name $(TEST_DB) deploy


setup_pgtap:
	@psql --dbname $(TEST_DB) --command 'CREATE EXTENSION pgtap;'
	@psql --dbname $(TEST_DB) --command 'CREATE EXTENSION chef_pgtap;'

setup_tests:
	@psql --dbname $(TEST_DB) --command "CREATE USER pushy_test WITH PASSWORD 'password'"
	@psql --dbname $(TEST_DB) --command "GRANT ALL ON ALL TABLES IN SCHEMA public TO pushy_test"
	$(foreach file, $(TEST_FUNCTIONS), psql --dbname $(TEST_DB) --single-transaction --set ON_ERROR_STOP=1 --file $(file);)

setup_dev: setup_schema setup_tests

test:
	@echo "Executing pgTAP tests in database '$(TEST_DB)'"
	@pg_prove --dbname $(TEST_DB) --verbose t/pushy_server_schema.pg
