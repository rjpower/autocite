LATEX := xelatex --interaction=nonstopmode
PDFTEX := xelatex --interaction=nonstopmode
DEPS := $(shell ls *.tex *.bib)

all : paper.pdf

%.ps : %.dvi
	dvips -Pcmz -t letter -o ${@} $<

paper.pdf: $(DEPS) 
	$(LATEX) paper
	bibtex paper
	$(LATEX) paper
	$(PDFTEX) paper

clean:
	rm -f *.aux *.dvi *.log *.bbl* *.pdf *.blg *.ent *.pdf *.ps


.PHONY: all figures
