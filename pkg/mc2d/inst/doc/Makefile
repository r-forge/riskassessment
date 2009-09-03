TEXI2DVI = texi2dvi --pdf
SWEAVE = R CMD Sweave
RM = rm -rf

all: docmcEnglish.pdf clean

noclean: docmcEnglish.pdf

docmcEnglish.pdf : docmcEnglish.Rnw
	$(SWEAVE) docmcEnglish.Rnw
	$(TEXI2DVI) docmcEnglish.tex

clean:
	$(RM) Rplots*.eps Rplots*.pdf Rplots*.jpg docmcEnglish-*.eps docmcEnglish-*.pdf docmcEnglish-*.jpg *.aux *.log *.blg *.bbl *.brf *.out *.tex *.rel *.toc *.bib *.sty *~ auto

allclean: 
	$(RM) Rplots*.eps Rplots*.pdf Rplots*.jpg docmcEnglish-*.eps docmcEnglish-*.pdf docmcEnglish-*.jpg *.aux *.log *.blg *.bbl *.brf *.out *.tex *.rel *.toc *.bib *.sty *~ auto docmcEnglish.pdf
