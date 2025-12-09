.PHONY: all re clean fclean

all:
	cabal build
	cp $(shell cabal list-bin exe:lisp-interpreter) .

clean:
	cabal clean

test:
	cabal test all

coverage:
	cabal test --enable-coverage

fclean: clean

.NOTPARALLEL: re
re: clean all

%:
	cabal $@
