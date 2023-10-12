SOURCES = $(shell find -type f -name "*.java")
CLASSES = $(patsubst src/%.java,out/%.class,$(SOURCES))
ANTLR_SOURCES = $(shell find -type f -name "*.g4")
ANTLR_RESULTS = $(patsubst %.g4,%Parser.java,$(ANTLR_SOURCES))
ANTLR = lib/antlr-4.7.1-complete.jar
MAINCLASS = Main

all: $(ANTLR_RESULTS) $(CLASSES)

run: cp
	java -Xmx256M -Xms256M -ea Main

pack:
	rm hw0.zip && zip hw0.zip -r Makefile *.java expression/*.java parser/*.java utils/*.java

clean:
	rm -rf out
	rm -f src/parser/*.java

cp:
	javac $(SOURCES)

out:
	mkdir -p out