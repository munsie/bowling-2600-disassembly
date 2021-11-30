NAME = Bowling
ROM = $(NAME).bin
ROM_MD5 = c9b7afad3bfd922e006a6bfc1d4f3fe7

# override this to 0 to not fail the build when there is a MD5 mismatch 
FAIL_ON_MISMATCH = 1

DASM_DIR = $(HOME)/dev/dasm
DASM = $(DASM_DIR)/bin/dasm
Z26 = /Applications/z26mac.app/Contents/MacOS/z26mac
STELLA = /Applications/Stella.app/Contents/MacOS/Stella
MD5 = md5 -r

.PHONY: all
all: $(ROM)

%.bin: %.asm
	@echo "===> Building $< <==="
	@$(DASM) $< -I$(DASM_DIR)/machines/atari2600/ -f3 -o$@ -s$(basename $@).sym ; tmp=$$? ; if [ $$tmp -ne 0 ]; then rm -f $@ $(basename $@) ; exit $$tmp; fi
	@SUM="$$($(MD5) $(ROM) | cut -d' ' -f1)" && if [ "$$SUM" != '$(ROM_MD5)' ]; then echo "ROM Mismatch!"; exit $(FAIL_ON_MISMATCH); fi
	
.PHONY: run
run: $(ROM)
	$(Z26) -v11 -C80 $(ROM)

.PHONY: runfull
runfull: $(ROM)
	$(Z26) $(ROM)

.PHONY: debug
debug: $(ROM)
	$(STELLA) -debug $(ROM)

.PHONY: clean
clean:
	rm -rf *~ $(ROM) $(NAME).sym
