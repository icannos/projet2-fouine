all:
	ocamlbuild -yaccflag -v -lib unix main.native #on dit de fabriquer main.native
	mkdir -p ../bin
	cp -L main.native ../bin/fouine
	chmod +x ../bin/fouine

byte:
	ocamlbuild -yaccflag -v main.byte
	mkdir -p ../bin
	cp -L main.byte ../bin/fouine.byte

clean:
	ocamlbuild -clean
	rm -r -d ../bin/
