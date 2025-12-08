CABAL-EXTRACT := $(shell cabal -v0 list-bin exe:cabal-extract --dry-run)

SELF := $(firstword $(MAKEFILE_LIST))

cabal-cmd-lib = cabal build $(strip $1)
cabal-cmd-exe = cabal build $(strip $1)
cabal-cmd-test = cabal test $(strip $1)

all: glados

glados: cli\:cli
	cp $(shell cabal -v0 list-bin exe:cli) $@
	chmod +x $@

define mk-target
_name_$(strip $1) := $(subst \:library,,$(subst +,\:,$(strip $1)))

.PHONY: $$(_name_$(strip $1))

$$(_name_$(strip $1)):
	$$(call cabal-cmd-$($(strip $1)), $$(_name_$(strip $1)))

endef

all-targets +=
-include .build/types.mk

$(foreach target, $(all-targets), $(info $(call mk-target, $(target))))
$(eval $(foreach target, $(all-targets), \
	$(eval $(call mk-target, $(target)))))

.build/types.mk: .build/layout.json $(SELF)
	mkdir -p $(dir $@)
	jq -r '.[] | "\(.name)=\(.type)"' .build/layout.json | tr ':' '+' > $@
	grep -Po "^(.*)(?=[=])" $@ \
		| xargs -i echo "all-targets += {}" >> $@

.build/layout.json: $(CABAL-EXTRACT)
	mkdir -p $(dir $@)
	cabal run cabal-extract -- . > $@

$(CABAL-EXTRACT): cabal-extract
	cabal build cabal-extract

.PHONY: tests_run
tests_run:
	cabal test all

.PHONY: clean
clean:
	cabal clean

.PHONY: fclean
fclean: clean
