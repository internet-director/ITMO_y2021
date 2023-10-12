CXXFLAGS+=-std=c++14 -Wall -O2
CXX=g++

all: out out/main.o

run: out out/main.o
	./out/main.o

out/main.o: src/main.cpp src/Expression.cpp src/Parser.cpp
	$(CXX) $^ $(CXXFLAGS) -o out/main.o

pack:
	zip hw0.zip -r Makefile src

out:
	mkdir -p out

clean:
	rm -rf out