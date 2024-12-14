.PHONY: force

# BUILD=stack runhaskell --package hspec --
BUILD=stack ghc --package hspec --

%_build: %.hs force
	$(BUILD) -o out.bin $<

%_test: %_build
	./out.bin problem_3_test.txt

%_solve: %_build
	./out.bin problem_3_solve.txt
