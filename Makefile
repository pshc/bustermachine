TARGET = bm
TARGET_SRC := $(wildcard Nono/*.hs)
DIST_SRC := $(wildcard Buster/*.hs)
MAKE = make --no-print-directory

all: $(TARGET)

run: $(TARGET)
	./$<

$(TARGET): .dist $(TARGET_SRC) plugins
	@rm -f Nono/nono
	@$(MAKE) -C Nono nono
	@ln -sf Nono/nono $@

.dist: configure $(DIST_SRC)
	./Setup.lhs build
	./Setup.lhs install
	@touch $@

configure: bustermachine.cabal
	@touch configure
	./$@

plugins:
	@$(MAKE) -C plugins all

clean:
	./Setup.lhs clean
	@$(MAKE) -C Nono clean
	@$(MAKE) -C plugins clean
	rm -f -- .dist $(TARGET)

.PHONY: all clean plugins run
