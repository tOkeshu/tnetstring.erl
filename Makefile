all: tests

ebin:
	mkdir ebin

compile: ebin
	@cd ebin && erlc ../src/tnetstring.erl ../test/tnetstring_tests.erl

tests: compile
	@erl -noshell -pa ebin -eval "eunit:test(tnetstring_tests, [verbose])" -s init stop

