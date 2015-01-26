PAGES_TEX=$(wildcard *.tex)
PAGES_PDF=$(patsubst %.tex,%.pdf,$(PAGES_TEX))
PAGES_PNG=$(patsubst %.tex,%.png,$(PAGES_TEX))

PRESENTATION=rules.svg
.PHONY: all clean clean-latex newversion

all: $(PRESENTATION)

$(PRESENTATION): $(PAGES_PNG)

%.pdf : %.tex
	rubber -v -n -1 -I. -s --pdf $<

%.png : %.pdf
	convert -density 600 $< $@

clean: clean-latex
	rm -f $(PAGES_PNG)
	rm -f *~

clean-latex:
	rubber --clean $(PAGES_TEX)

NOW:=$(shell date +%Y-%m-%d-%H-%M)
newversion: $(PRESENTATION)
	mkdir -p versions/$(NOW)
	cp -ai $(PRESENTATION) $(PAGES_PNG) versions/$(NOW)/

