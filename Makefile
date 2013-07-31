.PHONY: all clean

OBJECTS = SimpleEchoExample

all: $(OBJECTS)

SimpleEchoExample:
	@type cabal-dev &> /dev/null || (echo "cabal-dev not found. Install Haskell Plaftorm and cabal-dev"; false)
	@cabal-dev configure
	@cabal-dev build
	@cp dist/build/SimpleEchoExample/SimpleEchoExample .

clean:
	$(RM) -r *.o *.hi $(OBJECTS) dist
