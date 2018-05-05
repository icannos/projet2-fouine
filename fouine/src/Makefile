all:
	ocamlbuild -yaccflag -v -lib unix main.native #on dit de fabriquer main.native
	mkdir -p ../bin
	cp -L main.native ../bin/fouine
	cp -L memfonc.ml ../bin/memfonc.ml
	chmod +x ../bin/fouine

byte:
	ocamlbuild -yaccflag -v main.byte
	mkdir -p ../bin
	cp -L main.byte ../bin/fouine.byte

clean:
	ocamlbuild -clean
	rm -r -d ../bin/
