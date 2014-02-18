OCAMLC=ocamlc
OCAMLOPT=ocamlopt
OCAMLDEP=ocamldep
OCAMLLEX=ocamllex
OCAMLYACC=ocamlyacc

APPLI_OBJS=types.cmo tools.cmo lexer.cmo parser.cmo eval.cmo output.cmo main.cmo 
BIG_APPLI_OBJS= types.cmo tools.cmo lexer.cmo parser.cmo bigEval.cmo output.cmo bigMain.cmo 
APPLI_INTERFACES= bigEval.cmi bigMain.cmi eval.cmi lexer.cmi output.cmi parser.cmi types.cmi main.cmi 
OTHERS_FILES=lexer.ml  parser.ml parser.mli

all: glowy bigGlowy

glowy:$(APPLI_OBJS)
	$(OCAMLC) -o glowy $^

bigGlowy:$(BIG_APPLI_OBJS)
	$(OCAMLC) -o bigGlowy $^

types.cmo:types.ml
	$(OCAMLC) -c $^
	
tools.cmo:tools.ml
	$(OCAMLC) -c $^

parser.ml:parser.mly
	$(OCAMLYACC) $^

parser.mli:parser.mly
	$(OCAMLYACC) $^

parser.cmi:parser.mli
	$(OCAMLC) -c $^

parser.cmo:parser.ml
	$(OCAMLC) -c $^

lexer.ml:lexer.mll
	$(OCAMLLEX) $^

lexer.cmo:lexer.ml parser.cmi
	$(OCAMLC) -c $<

eval.cmo: eval.ml
	$(OCAMLC) -c $^

bigEval.cmo:bigEval.ml
	$(OCAMLC) -c $^

output.cmo:output.ml
	$(OCAMLC) -c $^

bigMain.cmo:bigMain.ml
	$(OCAMLC) -c $^

main.cmo:main.ml
	$(OCAMLC) -c $^

clean:
	$(RM) $(APPLI_OBJS) $(BIG_APPLI_OBJS) $(APPLI_INTERFACES) $(OTHERS_FILES)
	
mrproper: clean
	$(RM) glowy bigGlowy
