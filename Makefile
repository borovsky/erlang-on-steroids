STEROIDS_VERSION="0.1"

all: compile dializer test docs

compile: 
	@echo "Compiling..."
	@make -sC"lib/steroids-$(STEROIDS_VERSION)" compile

dializer:
	@echo "Checking types..."
	@make -sC "lib/steroids-$(STEROIDS_VERSION)" dializer

docs:
	@echo "Generating docs..."
	@make -sC "lib/steroids-$(STEROIDS_VERSION)" docs

test: compile
	@echo "Testing..."
	@make -sC "lib/steroids-$(STEROIDS_VERSION)" test

clean:
	@echo "Cleaning..."
	@make -sC "lib/steroids-$(STEROIDS_VERSION)" clean

