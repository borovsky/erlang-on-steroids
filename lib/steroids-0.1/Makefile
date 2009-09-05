APPNAME=steroids
DOC_OPTS="[{todo, true}]"


all: compile dializer test docs

compile: 
	mkdir -p ebin
	mkdir -p test/ebin
	erl -make 
	cp -f src/*.app ebin/

clean:
	rm -rf ./app/ebin/*.*
	rm -rf ./ebin/*.*
	rm -rf ./doc/*.*
	rm -rf ./test/ebin/*.*
	rm -rf ./test/logs/*
	rm -rf ./test/*.beam

dializer:
	dialyzer --no_check_plt --src -r src

docs:
	erl -noshell -run edoc_run application "'$(APPNAME)'" '"."' '$(DOC_OPTS)' -s init stop

test: compile
	mkdir -p test/logs
	erl -sname ct  -s ct_run script_start -s erlang halt -pa `pwd`/ebin/ \
	    -include `pwd`/src `pwd`/include -dir test -logdir test/logs

