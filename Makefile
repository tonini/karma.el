EMACS = emacs
EMACSFLAGS =
CASK = cask
VERSION = `$(CASK) exec $(EMACS) --version | head -1`

NO_COLOR=\033[0m
INFO_COLOR=\033[1;32m
STAT_COLOR=\033[1;33m

.PHONY: info test

info:
	@ echo "\n$(INFO_COLOR)Installed Emacs info: $(NO_COLOR)\n"
	@ echo "  $(STAT_COLOR)[PATH]$(NO_COLOR)    = `which ${EMACS}`"
	@ echo "  $(STAT_COLOR)[VERSION]$(NO_COLOR) = ${VERSION}"

test: info
	@ echo "\n$(INFO_COLOR)Run tests: $(NO_COLOR)\n"
	$(CASK) exec $(EMACS) -batch -Q -l test/test-runner.el
