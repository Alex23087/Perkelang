PREFIX ?= /usr/local/perkelang

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
	./_build/default/bin/perkc.exe test/test.perk
	gcc -o test/test.out test/test.c
	./test/test.out

.PHONY: extensions
extensions:
	cd perkelang-extension && \
	vsce package --allow-missing-repository
	cd perkelang-vscode-lsp && \
	npx tsc && \
	vsce package --allow-missing-repository

.PHONY: install
install: build uninstall
	cd _build/default/ && \
	sudo mkdir -p $(PREFIX)/ && \
	sudo cp -r . $(PREFIX)/ && \
	sudo ln -s $(PREFIX)/bin/perkc.exe /usr/local/bin/perkc

.PHONY: uninstall
uninstall:
	sudo rm -rf $(PREFIX)
	sudo rm -f /usr/local/bin/perkc