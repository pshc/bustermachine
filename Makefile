all: bm

run: bm
	./$<

bm: Buster.hs IRC.hs Misc.hs UNIX.hs
	ghc -o $@ --make $^

clean:
	rm -f -- *.hi *.o bm
