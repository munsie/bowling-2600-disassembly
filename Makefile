NAME = Bowling

NTSC_ROM = $(NAME)-NTSC.bin
NTSC_ROM_MD5 = c9b7afad3bfd922e006a6bfc1d4f3fe7

PAL_ROM = $(NAME)-PAL.bin
PAL_ROM_MD5 = f69bb58b815a6bdca548fa4d5e0d5a75

# override this to 0 to not fail the build when there is a MD5 mismatch 
FAIL_ON_MISMATCH = 0

DASM_DIR = $(HOME)/dev/dasm
DASM = $(DASM_DIR)/bin/dasm
Z26 = /Applications/z26mac.app/Contents/MacOS/z26mac
STELLA = /Applications/Stella.app/Contents/MacOS/Stella
MD5 = md5 -r

.PHONY: all
all: $(NTSC_ROM)

$(NTSC_ROM): $(NAME).asm
	@echo "===> Building $@ <==="
	@$(DASM) $< -I$(DASM_DIR)/machines/atari2600/ -f3 -o$@ -s$(basename $@).sym ; tmp=$$? ; if [ $$tmp -ne 0 ]; then rm -f $@ $(basename $@) ; exit $$tmp; fi
	@SUM="$$($(MD5) $@ | cut -d' ' -f1)" && if [ "$$SUM" != '$(NTSC_ROM_MD5)' ]; then echo "ROM Mismatch!"; exit $(FAIL_ON_MISMATCH); fi
	
$(PAL_ROM): $(NAME).asm
	@echo "===> Building $@ <==="
	@$(DASM) $< -I$(DASM_DIR)/machines/atari2600/ -f3 -DPAL -o$@ -s$(basename $@).sym ; tmp=$$? ; if [ $$tmp -ne 0 ]; then rm -f $@ $(basename $@) ; exit $$tmp; fi
	@SUM="$$($(MD5) $@ | cut -d' ' -f1)" && if [ "$$SUM" != '$(PAL_ROM_MD5)' ]; then echo "ROM Mismatch!"; exit $(FAIL_ON_MISMATCH); fi
	
.PHONY: run
run: $(NTSC_ROM)
	$(Z26) -v11 -C80 $(NTSC_ROM)

.PHONY: run-pal
run-pal: $(PAL_ROM)
	$(Z26) -v11 -C80 $(PAL_ROM)

.PHONY: runfull
runfull: $(NTSC_ROM)
	$(Z26) $(NTSC_ROM)

.PHONY: runfull-pal
runfull-pal: $(PAL_ROM)
	$(Z26) $(PAL_ROM)

.PHONY: debug
debug: $(NTSC_ROM)
	$(STELLA) -debug $(NTSC_ROM)

.PHONY: debug-pal
debug-pal: $(PAL_ROM)
	$(STELLA) -debug $(PAL_ROM)

.PHONY: clean
clean:
	rm -rf *~ $(NTSC_ROM) $(PAL_ROM) *.sym
