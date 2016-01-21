.SUFFIXES: .lhs .mkd .html .tex .pdf

PANDOC := pandoc  -sS
HSCOLOUR := hscolour -lit

SED_REMOVE_HIDDEN := sed '/%if False/,/%endif/d'
SED_CONVERT := sed '/\\begin{code}/,/\\end{code}/s/^/> /g'
GREP_FILTER := grep -ve '\\\(begin\|end\){code}'

.lhs.mkd:
	cat $< | $(SED_REMOVE_HIDDEN) | $(SED_CONVERT) | $(GREP_FILTER) | $(HSCOLOUR) -css > $@

.lhs.html:
	cat $< | $(SED_REMOVE_HIDDEN) | $(SED_CONVERT) | $(GREP_FILTER) | $(HSCOLOUR) -css | $(PANDOC) -t html -c hscolour.css > $@

.lhs.tex:
	cat $< | $(HSCOLOUR) -latex | $(PANDOC) -t latex> $@

.tex.pdf:
	pdflatex $< && pdflatex $< && pdflatex $<
