all: nono

ifneq ($(shell uname),Darwin)
 LINKFLAGS = -dynamic
endif

nono: Nono.hs
	ghc -o $@ --make $^ $(LINKFLAGS)

clean:
	rm -f -- *.hi *.o nono
