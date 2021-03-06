STEROIDS_VERSION="0.1"
ERLYDTL_VERSION="0.1"

all: compile test

full: compile dializer test_full docs

compile: 
	@echo "Compiling..."
	@make -sC"lib/steroids-$(STEROIDS_VERSION)" compile
	@make -sC"lib/erlydtl-ng-$(ERLYDTL_VERSION)" compile

dializer:
	@echo "Checking types..."
	@make -sC "lib/steroids-$(STEROIDS_VERSION)" dializer

docs:
	@echo "Generating docs..."
	@make -sC "lib/steroids-$(STEROIDS_VERSION)" docs

test: compile
	@echo "Testing..."
	@make -sC "lib/steroids-$(STEROIDS_VERSION)" test

test_full: compile
	@echo "Testing..."
	@make -sC "lib/steroids-$(STEROIDS_VERSION)" test
	@make -sC"lib/erlydtl-ng-$(ERLYDTL_VERSION)" test

clean:
	@echo "Cleaning..."
	@make -sC "lib/steroids-$(STEROIDS_VERSION)" clean
	@make -sC"lib/erlydtl-ng-$(ERLYDTL_VERSION)" clean

