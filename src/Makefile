all: exec doc

	
exec:
	ocamlbuild -yaccflag -v -lib unix main.native #on dit de fabriquer main.native
	mkdir -p ../bin
	cp -L main.native ../bin/fouine
	sed -E "s/\(\*.*\*\)//g"  memfonc.ml > ../bin/memfonc.ml
	chmod +x ../bin/fouine

	
doc:
	mkdir -p ../doc/latex
	mkdir -p ../doc/html
	ocamldoc -html -I _build/ *.ml -d ../doc/html/ -charset utf8
	ln -sf ../doc/html/index.html ../doc/index.html
	ocamldoc -latex -I _build/ *.ml -o ../doc/latex/fouine_documentation.out 
	sed -i 's/latin1/utf8/' ../doc/latex/fouine_documentation.out
	- pdflatex -interaction=nonstopmode -output-directory ../doc/latex/ ../doc/latex/fouine_documentation.out > /dev/null	
	ln -sf ../doc/latex/fouine_documentation.pdf ../doc/fouine_documentation.pdf
	
byte:
	ocamlbuild -yaccflag -v main.byte
	mkdir -p ../bin
	cp -L main.byte ../bin/fouine.byte

clean:
	ocamlbuild -clean
	rm -f -d ../bin/
	rm -f ../doc/fouine_documentation.pdf
	rm -f ../doc/index.html
	rm -f -r -d ../doc/latex/
	rm -f -r -d ../doc/html/

