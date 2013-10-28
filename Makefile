EMACS = emacs
EMACSFLAGS =
CASK = cask
VERSION = `$(CASK) exec $(EMACS) --version | head -1`
KARMA = karma.el

NO_COLOR=\033[0m
INFO_COLOR=\033[2;32m
STAT_COLOR=\033[2;33m

.PHONY: info test build compile cask

info:
	@ echo "\n$(INFO_COLOR)Installed Emacs info: $(NO_COLOR)\n"
	@ echo "  $(STAT_COLOR)[PATH]$(NO_COLOR)    = `which $(EMACS)`"
	@ echo "  $(STAT_COLOR)[VERSION]$(NO_COLOR) = $(VERSION)"

test:
	@ echo "\n$(INFO_COLOR)Run tests: $(NO_COLOR)\n"
	$(CASK) exec $(EMACS) -batch -Q -l test/test-runner.el

build: cask compile test

cask:
	@ echo "\n$(INFO_COLOR)Install package dependencies: $(NO_COLOR)\n"
	@ echo "$(STAT_COLOR)[cask install]$(NO_COLOR)"
	@ echo `$(CASK) install`
	@ echo "$(STAT_COLOR)[cask update]$(NO_COLOR)"
	@ echo `$(CASK) update`

compile:
	@ echo "\n$(INFO_COLOR)Compile: $(NO_COLOR)\n"
	@ echo "$(STAT_COLOR)[$(CASK) exec $(EMACS) -Q -batch -f batch-byte-compile $(KARMA)]$(NO_COLOR)"
	@ echo `$(CASK) exec $(EMACS) -Q -batch -f batch-byte-compile $(KARMA)`
