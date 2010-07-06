TARGET = nono
TARGET_SRC := $(wildcard Nono/*.hs)
DIST_SRC := $(wildcard Buster/*.hs) $(wildcard Buster/Machine/*.hs)
MAKE = make --no-print-directory

all: $(TARGET)

run: $(TARGET)
	./$<

$(TARGET): .dist-install $(TARGET_SRC)
	@$(MAKE) -C Nono $@
	@ln -sf Nono/$@

.dist: configure $(DIST_SRC)
	./Setup.lhs build
	@touch $@

.dist-install: .dist
	./Setup.lhs install
	@touch $@

configure: bustermachine.cabal
	@echo "./Setup.lhs configure --user" > $@
	@chmod a+x $@
	./$@

clean:
	./Setup.lhs clean
	@$(MAKE) -C Nono clean
	rm -f -- .dist .dist-install $(TARGET)

.PHONY: all clean run
