.PHONY: force

BUILD=stack ghc --package hspec --

%_build: %.hs force
	$(BUILD) -o out.bin $<

%_test: %_build
	./out.bin day_test.txt

%_solve: %_build
	./out.bin day_solve.txt
