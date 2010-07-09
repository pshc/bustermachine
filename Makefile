TARGET = bm
TARGET_SRC := $(wildcard Nono/*.hs)
DIST_SRC := $(wildcard Buster/*.hs) $(wildcard Buster/Machine/*.hs)
MAKE = make --no-print-directory

all: $(TARGET)

run: $(TARGET)
	./$<

$(TARGET): .dist $(TARGET_SRC)
	@rm -f Nono/nono
	@$(MAKE) -C Nono nono
	@ln -sf Nono/nono $@

.dist: configure $(DIST_SRC)
	./Setup.lhs build
	./Setup.lhs install
	@touch $@

configure: bustermachine.cabal
	@echo "./Setup.lhs configure --user" > $@
	@chmod a+x $@
	./$@

clean:
	./Setup.lhs clean
	@$(MAKE) -C Nono clean
	rm -f -- .dist $(TARGET)

.PHONY: all clean run
