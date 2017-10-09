PRINCIPALE		= Simo
PRINCIPALE_TEX		= $(PRINCIPALE).tex
PRINCIPALE_PDF		= $(PRINCIPALE).pdf

. PHONY : clean
clean :
	rm -f *.aux *.out *.snm *.toc *.nav regionsje.*

#
handout: $(PRINCIPALE_PDF)
	pdflatex -output-directory=../handouts "\def\ishandout{1} \input{$(PRINCIPALE_TEX)}"  
	rm -f ./handouts/*.aux ./handouts/*.out ./handouts/*.snm ./handouts/*.toc ./handouts/*.nav

presentazione: $(PRINCIPALE_PDF)
	pdflatex $(PRINCIPALE_TEX)

note: $(NOTE)
	pdflatex -output-directory=./handouts/note "\def\isNote{1} \input{$(PRINCIPALE_TEX)}"  

