all: fouine report

	

fouine:
	cd src;	make; cd ..

	
report:
	cd rapport;\
	pdflatex -interaction=nonstopmode rapport.tex > /dev/null; \
	bibtex rapport > /dev/null; \
	pdflatex -interaction=nonstopmode rapport.tex > /dev/null; \
	pdflatex -interaction=nonstopmode rapport.tex > /dev/null; \
	pdflatex -interaction=nonstopmode presentation.tex > /dev/null; \
	
clean:
	cd rapport; latexmk -CA; rm -f presentation.nav; rm -f presentation.snm; rm -f rapport.bbl
	cd src;	make clean
