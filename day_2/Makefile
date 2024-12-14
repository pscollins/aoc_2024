.PHONY: force

%_build: %.hs force
	ghc -o out.bin $<

%_test: %_build
	./out.bin problem_3_test.txt

%_solve: %_build
	./out.bin problem_3_solve.txt
