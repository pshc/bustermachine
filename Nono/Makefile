all: nono

nono: Nono.hs
	ghc -o $@ -dynamic --make $^

clean:
	rm -f -- *.hi *.o nono
