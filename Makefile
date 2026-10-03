include epkg.mk
epkg.mk:
	emacs --batch -l package -f package-initialize -l epkg -f epkg-copy-mk

SHELL := /bin/bash
EMACS ?= emacs
TESTSRC := test-project-claude.el test-project-gemini.el
ELGEN := project-claude.el project-gemini.el project-claude-generated.el project-gemini-generated.el
TESTGEN := test-project-claude-generated.el test-project-gemini-generated.el

EPKG_EL := $(ELGEN)
EPKG_FILES := $(EPKG_EL)
EPKG_MAIN := project-claude.el
EPKG_TEST_EL := $(TESTSRC)

TEMU ?= ghostty

ifeq ($(TEMU),ghostty)
TEMU_PKG  := ghostty-vt
TEMU_DIR  := epkg/ghostty-vt
TEMU_REPO := https://github.com/dickmao/emacs-ghostty
else
TEMU_PKG  := vterm
TEMU_DIR  := epkg/vterm
TEMU_REPO := https://github.com/commercial-emacs/emacs-libvterm
endif

.DEFAULT_GOAL := compile

.PHONY: compile
compile: $(ELGEN) $(TESTGEN) epkg-compile

project-claude.el: project-claude-template.el
	sed -e 's|@TEMU_PKG@|$(TEMU_PKG)|g' \
	    project-claude-template.el > project-claude.el

project-gemini.el: project-gemini-template.el
	sed -e 's|@TEMU_PKG@|$(TEMU_PKG)|g' \
	    project-gemini-template.el > project-gemini.el

project-claude-generated.el: template.el
	sed -e 's|@PROVIDER@|claude|g' \
	    -e 's|@PROVIDER_TITLE@|Claude Code|g' \
	    -e 's|@TEMU_PKG@|$(TEMU_PKG)|g' \
	    -e 's|@TEMU_REPO@|$(TEMU_REPO)|g' \
	    template.el > project-claude-generated.el

project-gemini-generated.el: template.el
	sed -e 's|@PROVIDER@|gemini|g' \
	    -e 's|@PROVIDER_TITLE@|Gemini CLI|g' \
	    -e 's|@TEMU_PKG@|$(TEMU_PKG)|g' \
	    -e 's|@TEMU_REPO@|$(TEMU_REPO)|g' \
	    template.el > project-gemini-generated.el

test-project-claude-generated.el: test-template.el
	sed -e 's|@PROVIDER@|claude|g' \
	    -e 's|@TEMU_PKG@|$(TEMU_PKG)|g' \
	    -e 's|@TEMU_DIR@|$(TEMU_DIR)|g' \
	    test-template.el > test-project-claude-generated.el

test-project-gemini-generated.el: test-template.el
	sed -e 's|@PROVIDER@|gemini|g' \
	    -e 's|@TEMU_PKG@|$(TEMU_PKG)|g' \
	    -e 's|@TEMU_DIR@|$(TEMU_DIR)|g' \
	    test-template.el > test-project-gemini-generated.el

.PHONY: test
test: compile epkg-test

.PHONY: clean
clean:
	git clean -dfX

.PHONY: veryclean
veryclean: clean
	git clean -dffX # removes TEMUs

.PHONY: install
install: $(ELGEN) epkg-install
