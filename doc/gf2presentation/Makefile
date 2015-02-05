PAGES_TEX=$(wildcard *.tex)
PAGES_PDF=$(patsubst %.tex,%.pdf,$(PAGES_TEX))
PAGES_PNG=$(patsubst %.tex,%.png,$(PAGES_TEX))

PRESENTATION=rules.svg
.PHONY: all clean clean-latex newversion
.PRECIOUS: $(PAGES_PDF)

all: $(PRESENTATION)

$(PRESENTATION): $(PAGES_PNG)

%.pdf : %.tex
	TEXINPUTS=.:./latexstuff/: pdflatex $<

%.png : %.pdf
	convert -density 600 $< $@

clean: clean-latex
	rm -f $(PAGES_PNG)
	rm -f $(PAGES_PDF)
	rm -f *~

clean-latex:
	rm -f *.toc *.log *.aux *.tmp *.dvi *.bbl *.blg *.out

NOW:=$(shell date +%Y-%m-%d-%H-%M)
newversion: $(PRESENTATION)
	mkdir -p versions/$(NOW)
	cp -ai $(PRESENTATION) $(PAGES_PNG) versions/$(NOW)/

