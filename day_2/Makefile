.PHONY: force

%_build: %.hs force
	ghc -o out.bin $<

%_test: %_build
	./out.bin $*_test.txt

%_solve: %_build
	./out.bin $*_solve.txt
