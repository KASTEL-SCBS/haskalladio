.SUFFIXES: .lhs .mkd .html .tex .pdf
.PHONY: all clean

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

clean:
	rm -f $(HTMLS)
