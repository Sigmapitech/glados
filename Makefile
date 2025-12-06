.PHONY: all re clean fclean

all:
	cabal build
	cp $(shell cabal list-bin exe:lisp-interpreter) .

test:
	cd lisp && cabal test

coverage:
	cd lisp && cabal test --enable-coverage
	find lisp/dist-newstyle -name "hpc_index.html" -type f 2>/dev/null | head -1

clean:
	cabal clean

fclean: clean

.NOTPARALLEL: re
re: clean all

%:
	cabal $@
