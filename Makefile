.PHONY: all re clean fclean

all:
	cabal build
	cp $(shell cabal list-bin exe:lisp-interpreter) .

clean:
	cabal clean

fclean: clean

.NOTPARALLEL: re
re: clean all

%:
	cabal $@
