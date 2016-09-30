.SUFFIXES: .lhs .mkd .html .tex .pdf
.PHONY: all clean test testGreiner testComparison .FORCE

CABAL_PREFIX=cabal exec --


PANDOC := pandoc  -sS --include-in-header=hscolour.css
HSCOLOUR := hscolour -lit

SED_REMOVE_HIDDEN := sed '/%if False/,/%endif/d'
SED_CONVERT := sed '/\\begin{code}/,/\\end{code}/s/^/> /g'
GREP_FILTER := grep -ve '\\\(begin\|end\){code}'

HTMLS = Noninterference.html NoninterferenceGreiner.html Noninterference/Procedure.html Instances/PaperExample/ExampleOne/Noninterference.html
all : $(HTMLS)

.lhs.mkd:
	cat $< | $(SED_REMOVE_HIDDEN) | $(SED_CONVERT) | $(GREP_FILTER) | $(HSCOLOUR) -css > $@

.lhs.html:
	cat $< | $(SED_REMOVE_HIDDEN) | $(SED_CONVERT) | $(GREP_FILTER) | $(HSCOLOUR) -css | $(PANDOC) -t html -c hscolour.css > $@

.lhs.tex:
	cat $< | $(HSCOLOUR) -latex | $(PANDOC) -t latex> $@

.tex.pdf:
	pdflatex $< && pdflatex $< && pdflatex $<


%.pdf : %.svg
	inkscape -f $< -A $@ --export-dpi=360

Noninterference/examples-include.tex : Noninterference/Examples.hs
	$(CABAL_PREFIX) runghc $< > $@


Noninterference/KASTELTalk-examples-include.tex : Noninterference/ExamplesKASTELTalk.hs Noninterference/Export.hs
	$(CABAL_PREFIX) runghc $< > $@

IMAGES_SVGS=$(wildcard Noninterference/img/svg/*.svg)
IMAGES_PDFS=$(patsubst %.svg,%.pdf,$(IMAGES_SVGS))
Noninterference/ExamplesKASTELTalk.pdf : Noninterference/ExamplesKASTELTalk.tex Noninterference/KASTELTalk-examples-include.tex $(IMAGES_PDFS)
	cd Noninterference/ && rubber --pdf ExamplesKASTELTalk.tex




test.bin : .FORCE
	$(CABAL_PREFIX) ghc $(THREADED) -rtsopts -O --make Noninterference.Test  -main-is Noninterference.Test -o $@

testGreiner.bin : .FORCE
	$(CABAL_PREFIX) ghc $(THREADED) -rtsopts -O --make Noninterference.TestGreiner  -main-is Noninterference.TestGreiner -o $@

testComparison.bin : .FORCE
	$(CABAL_PREFIX) ghc $(THREADED) -rtsopts -O --make Noninterference.TestComparison  -main-is Noninterference.TestComparison -o $@

test : test.bin .FORCE
	./$< $(RTS)

testGreiner : testGreiner.bin .FORCE
	./$< $(RTS)

testComparison : testComparison.bin .FORCE
	./$< $(RTS)

clean:
	rm -f $(HTMLS)
	find -name "*.hi"      -not -path "./.cabal-sandbox/*" -delete
	find -name "*.dyn_hi"  -not -path "./.cabal-sandbox/*" -delete
	find -name "*.o"       -not -path "./.cabal-sandbox/*" -delete
	find -name "*.dyn_o"   -not -path "./.cabal-sandbox/*" -delete
	find -name "*~"        -not -path "./.cabal-sandbox/*" -delete
	rm -f test.bin testGreiner.bin testComparison.bin
	rm -f Noninterference/examples-include.tex
	rm -f tmpfile*
