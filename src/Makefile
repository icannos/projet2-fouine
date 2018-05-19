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
	ocamldoc -latex -I _build/ *.ml -o ../doc/latex/fouine_documentation.out 
	sed -i -e 's/\\`a/\d224/g' -e "s/\\\^a/\d226/g" -e "s/\\\'e/\d233/g" -e 's/\\`e/\d232/g' -e "s/\\\^e/\d234/g" -e 's/\\\"e/\d235/g' -e "s/\\\^o/\d244/g" -e 's/\\\"o/\d246/g' -e "s/\\\^i/\d238/g" -e 's/\\\"i/\d239/g' -e 's/\\`u/\d249/g' -e "s/\\\^u/\d251/g" -e "s/\\\c{c}/\d231/g" ../doc/latex/fouine_documentation.out 
	pdflatex -interaction=nonstopmode -output-directory ../doc/latex/ ../doc/latex/fouine_documentation.out 
	
byte:
	ocamlbuild -yaccflag -v main.byte
	mkdir -p ../bin
	cp -L main.byte ../bin/fouine.byte

clean:
	ocamlbuild -clean
	rm -r -d ../bin/
	rm -r -d ../doc/latex
	rm -r -d ../doc/html
	
