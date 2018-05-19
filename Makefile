all: fouine report

	

fouine:
	cd src;	make; cd ..

	
report:
	cd rapport;\
	pdflatex -interaction=nonstopmode rapport.tex > /dev/null; \
	bibtex rapport.tex > /dev/null; \
	pdflatex -interaction=nonstopmode rapport.tex > /dev/null; \
	pdflatex -interaction=nonstopmode rapport.tex > /dev/null; \
	pdflatex -interaction=nonstopmode presentation.tex > /dev/null
	
clean:
	cd rapport; latexmk -CA
	cd src;	make clean
