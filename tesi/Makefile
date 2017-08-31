PRINCIPALE		= Tesi_GIT
PRINCIPALE_TEX		= $(PRINCIPALE).tex

. PHONY : pulisci
pulisci :
	rm -f *.aux *.lof *.out *.snm *.toc *.bbl *.log *.bbl *.blg *.bcf $(PRINCIPALE)-frn.tex

#
Sweave:
	Rscript -e "Sweave('$(PRINCIPALE).Rnw', driver=RweaveLatex(), encoding = 'utf8')"
	Rscript -e "Sweave('$(PRINCIPALE).Rnw', driver=Rtangle(), encoding = 'utf8')"
	pdflatex $(PRINCIPALE)
	pdflatex $(PRINCIPALE)-frn.tex
	pdflatex $(PRINCIPALE_TEX)
	xreader $(PRINCIPALE).pdf


biblio:
	pdflatex $(PRINCIPALE_TEX)
	biber $(PRINCIPALE)
	pdflatex $(PRINCIPALE_TEX)

pdf:
	pdflatex $(PRINCIPALE)
	pdflatex $(PRINCIPALE)

tutto:
	Rscript -e "Sweave('$(PRINCIPALE).Rnw', driver=RweaveLatex(), encoding = 'utf8')"
	Rscript -e "Sweave('$(PRINCIPALE).Rnw', driver=Rtangle(), encoding = 'utf8')"
	pdflatex $(PRINCIPALE)
	biber $(PRINCIPALE)
	pdflatex $(PRINCIPALE)
	pdflatex $(PRINCIPALE)
	pdflatex $(PRINCIPALE)-frn.tex
	pdflatex $(PRINCIPALE_TEX)
	xreader $(PRINCIPALE).pdf


