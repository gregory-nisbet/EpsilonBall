# This makefile is just a wrapper around
# the stack command line interface. Commands
# should be as primitive as possible.

# unfiltered test output
verbosecheck:
	stack test

check:
	@bash ./scripts/checkfiltered.bash
