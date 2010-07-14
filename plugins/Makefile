PLUGIN_SRC := $(wildcard *.hs)
PLUGINS := $(patsubst %.hs,bin/%,$(PLUGIN_SRC))

all: bin $(PLUGINS)

bin/%: %.hs ../.dist
	ghc -o $@ -dynamic --make $<

bin:
	mkdir bin

clean:
	rm -f -- *.hi *.o $(PLUGINS)
