name=amp

%.sym : %.tsym
	tragesym $< $@

SYMS = $(wildcard sym/*.tsym)
SYMBOLS := $(SYMS:.tsym=.sym)

symbols : $(SYMBOLS)

check-symbols : $(SYMBOLS)
	gsymcheck $(SYMBOLS)

.PHONY : clean
clean :
	rm -f $(SYMBOLS)

pcb : symbols
	gsch2pcb -v project | tee pcb.log

%.ps : %.pcb
	pcb -x ps --psfile $@ $<

%.pdf : %.ps
	ps2pdf $< $@

.PHONY : gerbers
gerbers : $(name).pcb $(name).bom
	rm -Rf gerbers
	mkdir gerbers
	pcb -x gerber --gerberfile gerbers/$(name) $<

gerbers.zip : gerbers
	rm -f $@
	zip -j $@ gerbers/*

.PHONY : osh-park-gerbers
osh-park-gerbers : gerbers
	mkdir -p $@
	cp gerbers/$(name).top.gbr "$@/Top Layer.ger"
	cp gerbers/$(name).bottom.gbr "$@/Bottom Layer.ger"
	cp gerbers/$(name).topmask.gbr "$@/Top Solder Mask.ger"
	cp gerbers/$(name).bottommask.gbr "$@/Bottom Solder Mask.ger"
	cp gerbers/$(name).topsilk.gbr "$@/Top Silk Screen.ger"
	cp gerbers/$(name).bottomsilk.gbr "$@/Bottom Silk Screen.ger"
	cp gerbers/$(name).outline.gbr "$@/Board Outline.ger"
	cp gerbers/$(name).plated-drill.cnc "$@/Drills.xln"

osh-park-gerbers.zip : osh-park-gerbers
	rm -f $@
	zip -j $@ osh-park-gerbers/*

%.bom : %.sch
	   gnetlist -g partslist3 -o $@ $<

