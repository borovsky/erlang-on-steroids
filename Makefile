all: compile test

compile: 
	mkdir -p ebin
	mkdir -p test/ebin
	erl -make 

clean:
	rm -rf ./ebin/*.*
	rm -rf ./test/ebin/*.*

test: compile
	erl -noshell -pa ebin -pa test/ebin -s erunit_suite run  -s init stop

