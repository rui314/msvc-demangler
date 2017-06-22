CXX=clang++
CXXFLAGS=-std=c++11

undname: MicrosoftDemangle.o
	$(CXX) -o $@ $?

test: undname
	@./runtest

.PHONY: test
