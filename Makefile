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

.PHONY: run_perf
run_perf:
	opam exec -- dune build
	# OCAMLRUNPARAM=b ./_build/default/bin/perkc.exe test/test.perk
	# gcc -o test/test.out test/test.c
	# ./test/test.out

	# perf record -F 1000 --call-graph dwarf ./_build/default/bin/perkc.exe ../super_perkio/src/main.perk
	./_build/default/bin/perkc.exe ../super_perkio/src/main.perk
	perf script -F +pid > test.perf
	gcc -o ../super_perkio/out/super_perkio ../super_perkio/src/main.c -lSDL2
	../super_perkio/out/super_perkio

.PHONY: debug_run
debug_run:
	opam exec -- dune build --profile=dev
	# OCAMLRUNPARAM=b ./_build/default/bin/perkc.exe test/normalexec/22-lambda_different_env.perk
	# gcc -o test/normalexec/22-lambda_different_env.out test/normalexec/22-lambda_different_env.c
	# ./test/normalexec/22-lambda_different_env.out

	OCAMLRUNPARAM=b ./_build/default/bin/perkc.exe ../super_perkio/src/main.perk
	gcc -o ../super_perkio/out/super_perkio ../super_perkio/src/main.c -lSDL2
	../super_perkio/out/super_perkio

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

.PHONY: test
test: build
	@COUNT=$$(ls -1 test/normalexec/*.perk | wc -l) ;\
	CURRENT=0 ;\
	IGNORE=(18) ;\
	for f in test/normalexec/*.perk ; \
	do \
		CURRENT=$$((CURRENT+1)) ;\
		if printf '%s\n' "$${IGNORE[@]}" | grep -q "^$$CURRENT$$"; then \
			# echo "[$$CURRENT/$$COUNT] Ignoring $$(basename "$${f%.*}")" ;\
			continue ;\
		fi ;\
		echo "[$$CURRENT/$$COUNT] Testing $$(basename "$${f%.*}")" ; \
		EXPECTED="$${f%.*}.expected" ;\
		RES=$$(_build/default/bin/perkc.exe "$$f" > /dev/null && gcc -Wno-int-to-pointer-cast -Wno-pointer-to-int-cast "$${f%.*}.c" -o "$$(dirname $$f)/a.out" && "$$(dirname $$f)/a.out") ; \
		rm -f "$$(dirname $$f)/a.out" ;\
		if [ $$? -eq 0 ]; then \
			# echo "$$RES" ;\
			if [ -e "$$EXPECTED" ]; then \
				echo "$$RES" | diff "$$EXPECTED" -;\
				if [ $$? -eq 0 ]; then \
					# rm -f "$${f%.*}.c" ;\
					:\
				else \
					echo "Test Failed";\
				fi ;\
			else \
				:;\
				# rm -f "$${f%.*}.c" ;\
				echo "$$RES" ;\
			fi;\
		else \
			echo "An error occurred while compiling $$(basename $${f%.*})" >&2;\
			echo "$$RES" >&2;\
		fi ;\
	done