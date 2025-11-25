.PHONY: all re clean fclean

CABAL_JSON := $(shell scripts/parse_cabal.py)
PKGS := $(shell scripts/parse_cabal.py | jq -r 'keys[]')

# Package-specific info
pkg_file = $(shell printf '%s' '$(CABAL_JSON)' | jq -r 'values[].file')
pkg_exes = $(shell printf '%s' '$(CABAL_JSON)' | jq -r 'values[].executables[]?' )

.PHONY: $(PKGS)

all: $(PKGS)
clean:
	cabal clean

fclean: clean
	rm -f $(foreach pkg,$(PKGS),$(pkg_exes))

.NOTPARALLEL: re
re: clean all


# Build a package
$(PKGS):
	@pkg="$@"; \
	echo "Building package $$pkg"; \
	cf=$$(printf '%s' '$(CABAL_JSON)' | jq -r --arg pkg "$$pkg" '.[$$pkg].file // empty'); \
	if [ -z "$$cf" ]; then echo "Package $$pkg not found"; exit 1; fi; \
	exes=$$(printf '%s' '$(CABAL_JSON)' | jq -r --arg pkg "$$pkg" '.[$$pkg].executables[]?'); \
	echo "Building $$pkg..."; \
	cabal build "$$pkg"; \
	echo $$exes; \
	for exe in $$exes; do \
		path=$$(cabal list-bin "$$pkg:exe:$$exe" 2>/dev/null || true); \
		if [ -n "$$path" ]; then cp "$$path" . || true; fi; \
	done

# Run tests (pattern rule)
%-test:
	@pkg="$*"; \
	if ! printf '%s\n' $(PKGS) | grep -Fxq "$$pkg"; then \
		echo "Error: unknown package '$$pkg'" >&2; exit 1; \
	fi; \
	echo "Testing $$pkg..."; \
	cabal test "$$pkg"

# Clean (pattern rule)
%-clean:
	@pkg="$*"; \
	if ! printf '%s\n' $(PKGS) | grep -Fxq "$$pkg"; then \
		echo "Error: unknown package '$$pkg'" >&2; exit 1; \
	fi; \
	echo "Cleaning (project) for $$pkg..."; \
	cabal clean

# Full clean for a package (pattern rule)
%-fclean:
	@pkg="$*"; \
	if ! printf '%s\n' $(PKGS) | grep -Fxq "$$pkg"; then \
		echo "Error: unknown package '$$pkg'" >&2; exit 1; \
	fi; \
	echo "Full cleaning for $$pkg..."; \
	exes=$$(printf '%s' '$(CABAL_JSON)' | jq -r --arg pkg "$$pkg" '.[$$pkg].executables[]?'); \
	for exe in $$exes; do rm -f "$$exe"; done; \
	echo "Running cabal clean..."; \
	cabal clean

# Rebuild (pattern rule)
%-re:
	@pkg="$*"; \
	if ! printf '%s\n' $(PKGS) | grep -Fxq "$$pkg"; then \
		echo "Error: unknown package '$$pkg'" >&2; exit 1; \
	fi; \
	echo "Rebuilding $$pkg..."; \
	$(MAKE) "$$pkg"-fclean && $(MAKE) "$$pkg"

# Fallback: forward unknown targets to cabal
%:
	@echo "Forwarding to cabal: $@"; cabal $@
