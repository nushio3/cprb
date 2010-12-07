all:test-1.out

.SUFFIXES: .o .cpp .cpprb

CXX = g++
CPRB = ../cprb

.cpprb.cpp:
	$(CPRB) $<

.cpp.o:
	$(CXX) -c $<

test-1.out:test-1.o
	$(CXX) -o $@ $^

clean:
	rm -f *.cpp *.out *.o *~

