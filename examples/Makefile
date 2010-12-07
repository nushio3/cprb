all: hello-world.out fruits.out collabo.out \
	test-1.out test-2.out test-3.out \
	test-assume-1.out test-assume-2.out

.SUFFIXES: .o .h .c .cpp .hrb .crb .cpprb

CC = gcc
CXX = g++
CPRB = ../cprb

.hrb.h:
	$(CPRB) $<
.crb.c:
	$(CPRB) $<
.cpprb.cpp:
	$(CPRB) $<


hello-world.out:hello-world.cpp
	$(CXX) -o $@ $^

fruits.out:fruits.cpp
	$(CXX) -o $@ $^

collabo.out:collabo.h collabo.c collabo-main.cpp
	$(CC) collabo.c -c
	$(CXX) collabo-main.cpp -c
	$(CXX) collabo.o collabo-main.o -o collabo.out


test-1.out:test-1.cpp
	$(CXX) -o $@ $^

test-2.out:test-2.cpp
	$(CXX) -o $@ $^

test-3.out:test-3.cpp
	$(CXX) -o $@ $^

test-assume-1.out:test-assume-1.cpp
	$(CXX) -o $@ $^

test-assume-2.out:test-assume-2.cpp
	$(CXX) -o $@ $^

.PHONY: clean
clean:
	rm -f *.c *.h *.cpp *.out *.o *~

