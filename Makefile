SOURCES = $(addprefix src/,$(shell ls src))
TESTS = $(addprefix test/,$(shell ls test))

.PHONY: all main clean test

all: main

main: $(SOURCES)
	ocamlbuild -I src -pkg str,easy-format main.native

test: $(SOURCES) $(TESTS)
	ocamlbuild -I src -I test -pkg str test_lexer.native
	ocamlbuild -I src -I test -pkg str test_parser.native
	ocamlbuild -I src -I test -pkg str test_typechecker.native
	ocamlbuild -I src -I test test_interpreter_while.native
	ocamlbuild -I src -I test test_interpreter_lambda.native
	ocamlbuild -I src -I test test_interpreter_arith.native

clean:
	rm -rf _build
	rm -f *.native