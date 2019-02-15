MODULES = board command main state UI_messages monte randomAI play score
OBJECTS = $(MODULES:=.cmo)
MLS = $(MODULES:=.ml)
MLIS = $(MODULES:=.mli)
TEST = test.byte
MAIN = main.byte
OCAMLBUILD = ocamlbuild -use-ocamlfind
PKGS = oUnit,ANSITerminal,unix,yojson

default: build
	utop

build:
	$(OCAMLBUILD) $(OBJECTS)

clean:
	ocamlbuild -clean

play:
	clear
	$(OCAMLBUILD) $(MAIN) && ./$(MAIN)

test:
	$(OCAMLBUILD) -tag 'debug' $(TEST) && ./$(TEST) -runner sequential

zip:
	zip ../src.zip *.ml* _tags Makefile *.json

docs: docs-public docs-private
	
docs-public: build
	mkdir -p doc.public
	ocamlfind ocamldoc -I _build -package $(PKGS) \
		-html -stars -d doc.public $(MLIS)

docs-private: build
	mkdir -p doc.private
	ocamlfind ocamldoc -I _build -package $(PKGS) \
		-html -stars -d doc.private \
		-inv-merge-ml-mli -m A -hide-warnings $(MLIS) $(MLS)
