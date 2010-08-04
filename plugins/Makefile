PLUGIN_SRC := $(wildcard *.hs)
PLUGINS := $(patsubst %.hs,bin/%,$(PLUGIN_SRC))

ifneq ($(shell uname),Darwin)
 LINKFLAGS = -dynamic
endif

all: bin $(PLUGINS)

bin/%: %.hs ../.dist
	ghc -o $@ --make $< $(LINKFLAGS)

bin:
	mkdir bin

clean:
	rm -f -- *.hi *.o $(PLUGINS)
