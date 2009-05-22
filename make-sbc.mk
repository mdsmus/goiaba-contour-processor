NAME = sbc2009
TEXSRCS = body-en.tex bibliography.tex abstract.tex
OTHER += $(LILY_PDF) $(SVG_PDF) $(GNUPLOT_PDF)

USE_PDFLATEX = 1

include ~/.latexmk

clean-partial:
	rm -f $(NAME).pdf
	rm -f $(NAME).aux
	rm -f $(NAME).log
	rm -f $(NAME).bbl
	rm -f $(NAME).blg
	rm -f $(NAME).out
