NAME = goiaba-contour-processor
TEXSRCS = body-en.tex bibliography.tex abstract.tex
OTHER += $(LILY_PDF) $(SVG_PDF) $(GNUPLOT_PDF)

USE_PDFLATEX = 1

include ~/.latexmk

