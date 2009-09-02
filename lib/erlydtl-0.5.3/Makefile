ERL=erl
ERLC=erlc

PARSER=src/erlydtl/erlydtl_parser
APP=erlydtl.app

all: compile

compile: $(PARSER).erl ebin/$(APP)
	$(ERL) -make 

ebin/$(APP): src/erlydtl/$(APP)
	-mkdir -p ebin
	cp $< $@

$(PARSER).erl: $(PARSER).yrl
	$(ERLC) -o src/erlydtl src/erlydtl/erlydtl_parser.yrl

run:
	$(ERL) -pa ebin

dializer:
	dialyzer --no_check_plt --src -r src/erlydtl


test: compile
	$(ERL) -noshell -pa ebin \
		-s erlydtl_functional_tests run_tests \
		-s erlydtl_dateformat_tests run_tests \
		-s erlydtl_unittests run_tests \
		-s init stop

clean:
	rm -f ebin/*.beam
	rm -f ebin/$(APP)
	rm -f erl_crash.dump $(PARSER).erl
	rm -f examples/rendered_output/*
