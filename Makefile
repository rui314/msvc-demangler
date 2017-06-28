CXX=clang++
CXXFLAGS=-std=c++11 -g -Wall

test: undname
	@./runtest

undname: MicrosoftDemangle.o
	$(CXX) -o $@ $?

clean:
	rm -f *.o *~ undname

.PHONY: test clean
