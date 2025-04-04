# .PHONY: compile
# compile:
# 	mkdir -p build
# 	rm -f build/*
# 	(opam exec -- dune test --force) 2> build/out.asm

.PHONY: build
build:
	opam exec dune build

.PHONY: clean
clean:
	rm -rf build
	opam exec dune clean
	rm -f test/test.out test/test.c

.PHONY: deps
deps:
	opam install ppx_deriving
	opam install sedlex
	opam install menhir

run: build
	./_build/default/bin/compiler.exe test/test.perk
	gcc -o test/test.out test/test.c
	./test/test.out

.PHONY: extension
extension:
	cd perkelang-extension && \
	vsce package --allow-missing-repository