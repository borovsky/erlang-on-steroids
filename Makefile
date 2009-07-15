all: compile test

compile: 
	mkdir -p ebin
	mkdir -p test/ebin
	erl -make 
	cp -f src/*.app ebin/

clean:
	rm -rf ./ebin/*.*
	rm -rf ./test/ebin/*.*
	rm -rf ./test/logs/*

test: compile
	run_test -dir test -logdir test/logs

