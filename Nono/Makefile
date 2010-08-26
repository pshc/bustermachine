all: nono

LINKFLAGS = -threaded
ifneq ($(shell uname),Darwin)
 LINKFLAGS := $(LINKFLAGS) -dynamic
endif

nono: Nono.hs
	ghc -o $@ --make $^ $(LINKFLAGS)

clean:
	rm -f -- *.hi *.o nono
