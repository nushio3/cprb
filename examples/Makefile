all: hello-world.out test-1.out test-2.out test-3.out

.SUFFIXES: .o .cpp .cpprb

CXX = g++
CPRB = ../cprb

.cpprb.cpp:
	$(CPRB) $<

hello-world.out:hello-world.cpp
	$(CXX) -o $@ $^

test-1.out:test-1.cpp
	$(CXX) -o $@ $^

test-2.out:test-2.cpp
	$(CXX) -o $@ $^

test-3.out:test-3.cpp
	$(CXX) -o $@ $^


.PHONY: clean
clean:
	rm -f *.cpp *.out *.o *~

