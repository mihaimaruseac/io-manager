.PHONY: all clean

OBJECTS = SimpleEchoExample

all: $(OBJECTS)

SimpleEchoExample:
	ghc --make -Wall -Werror $@

clean:
	$(RM) *.o *.hi $(OBJECTS)
