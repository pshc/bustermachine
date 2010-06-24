all: bm

run: bm
	./$<

bm: IRC.hs
	ghc -o $@ --make $^
