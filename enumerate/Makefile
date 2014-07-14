CXX=clang++
CXXFLAGS=-std=c++1y -stdlib=libc++ -O3 -Wall
CC=clang++
LDFLAGS=-stdlib=libc++

all: enumerate
enumerate.o: enumerate.cpp polytope.h
polytope.o: polytope.cpp polytope.h
enumerate: enumerate.o polytope.o
clean:
	rm -f polytope.o enumerate.o enumerate
