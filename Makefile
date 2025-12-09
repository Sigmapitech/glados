.PHONY: all
all: glados

SELF := $(firstword $(MAKEFILE_LIST))

glados: .build/mk.cli\:cli
	cp $(shell cabal -v0 list-bin exe:cli) $@
	chmod +x $@
	@ $(LOG_TIME) "Build $(C_CYAN)$(notdir $@)$(C_RESET)"

define mk-target
_name_$(strip $1) := $(subst \:library,,$(subst +,\:,$(strip $1)))

.PHONY: $$(_name_$(strip $1))

.build/mk.$$(_name_$(strip $1)): $$(shell git ls-files | grep ".*hs")
	$$(call cabal-cmd-$($(strip $1)), $$(_name_$(strip $1)))
	@ $$(LOG_TIME) "$$(C_CYAN)$$(_name_$(strip $1))$$(C_RESET)"
	@ touch $$@

$$(_name_$(strip $1)): .build/mk.$$(_name_$(strip $1))

endef

cabal-cmd-lib = cabal build $(strip $1)
cabal-cmd-exe = cabal build $(strip $1)
cabal-cmd-test = cabal test $(strip $1)

ifneq ($(MAKECMDGOALS),fclean)
CABAL-EXTRACT := $(shell cabal -v0 list-bin exe:cabal-extract --dry-run)

all-targets +=
-include .build/types.mk

# $(foreach target, $(all-targets), \
	$(info $(call mk-target, $(target))))

$(eval $(foreach target, $(all-targets), \
	$(eval $(call mk-target, $(target)))))
endif

.build/types.mk: .build/layout.json $(SELF)
	@ mkdir -p $(dir $@)
	@ jq -r '.[] | "\(.name)=\(.type)"' .build/layout.json | tr ':' '+' > $@
	@ grep -Po "^(.*)(?=[=])" $@ \
		| xargs -i echo "all-targets += {}" >> $@
	@ $(LOG_TIME) "Generated $(C_PURPLE)$@$(C_RESET)"

.build/layout.json: $(CABAL-EXTRACT)
	@ mkdir -p $(dir $@)
	@ cabal run cabal-extract -- . > $@

$(CABAL-EXTRACT): cabal-extract
	@ cabal build cabal-extract
	@ $(LOG_TIME) "Build $(C_CYAN)$(notdir $@)$(C_RESET)"

.PHONY: tests_run
tests_run:
	@ cabal test all

.PHONY: clean
clean:
	@ $(RM) .build/mk.*

.PHONY: fclean
test:
	cabal test all

coverage:
	cabal test --enable-coverage

fclean: clean
	@ cabal clean

.NOTPARALLEL: re
.PHONY: re
re: fclean all

ifneq ($(shell command -v tput),)
  ifneq ($(shell tput colors),0)

mk-color = \e[$(strip $1)m

C_BEGIN := \033[A
C_RESET := $(call mk-color, 00)

C_RED := $(call mk-color, 31)
C_GREEN := $(call mk-color, 32)
C_YELLOW := $(call mk-color, 33)
C_BLUE := $(call mk-color, 34)
C_PURPLE := $(call mk-color, 35)
C_CYAN := $(call mk-color, 36)

  endif
endif

NOW = $(shell date +%s%3N)

STIME := $(shell date +%s%3N)
export STIME

define TIME_MS
$$( expr \( $$(date +%s%3N) - $(STIME) \))
endef

BOXIFY = "[$(C_BLUE)$(1)$(C_RESET)] $(2)"

ifneq ($(shell command -v printf),)
  LOG_TIME = printf $(call BOXIFY, %6s , %b\n) "$(call TIME_MS)"
else
  LOG_TIME = echo -e $(call BOXIFY, $(call TIME_MS) ,)
endif
