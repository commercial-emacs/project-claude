SHELL := /bin/bash
EMACS ?= emacs
ELSRC := project-claude-package.el
TESTSRC := $(shell git ls-files test-project*.el)
ELGEN := project-claude.el project-gemini.el \
         project-claude-generated.el project-gemini-generated.el
TESTGEN := test-project-claude-generated.el test-project-gemini-generated.el
INSTALLDIR ?= package-user-dir
TEMU ?= ghostty

ifeq ($(TEMU),ghostty)
  ifeq ($(filter clean dist-clean,$(MAKECMDGOALS)),)
    ifneq ($(wildcard emacs-libvterm),)
      $(error emacs-libvterm exists: run make clean)
    endif
  endif
TEMU_PKG  := ghostty-vt
TEMU_DIR  := emacs-ghostty
TEMU_REPO := https://github.com/dickmao/emacs-ghostty.git
TEMU_EL   := $(TEMU_DIR)/ghostty-vt.el
TEMU_SO   := $(TEMU_DIR)/ghostty-vt-module.so
else
  ifeq ($(filter clean dist-clean,$(MAKECMDGOALS)),)
    ifneq ($(wildcard emacs-ghostty),)
      $(error emacs-ghostty exists: run make clean)
    endif
  endif
TEMU_PKG  := vterm
TEMU_DIR  := emacs-libvterm
TEMU_REPO := https://github.com/commercial-emacs/emacs-libvterm.git
TEMU_EL   := $(TEMU_DIR)/vterm.el
TEMU_SO   := $(TEMU_DIR)/vterm-module.so
endif

.PHONY: compile
compile: deps/archives/gnu/archive-contents $(ELGEN) $(TESTGEN)
	$(EMACS) -batch \
	  --eval "(setq byte-compile-error-on-warn t)" \
	  --eval "(setq package-user-dir (expand-file-name \"deps\"))" \
	  -f package-initialize \
	  -L . \
	  -f batch-byte-compile $(ELSRC) $(ELGEN) $(TESTSRC); \
	  (ret=$$? ; rm -f $(ELSRC:.el=.elc) $(ELGEN:.el=.elc) $(TESTSRC:.el=.elc) && exit $$ret)

project-claude.el: project-claude-template.el
	sed -e 's/@TEMU@/$(TEMU_PKG)/g' \
	    project-claude-template.el > project-claude.el

project-gemini.el: project-gemini-template.el
	sed -e 's/@TEMU@/$(TEMU_PKG)/g' \
	    project-gemini-template.el > project-gemini.el

project-claude-generated.el: template.el
	sed -e 's/@PROVIDER@/claude/g' \
	    -e 's/@PROVIDER_TITLE@/Claude Code/g' \
	    -e 's/@TEMU@/$(TEMU_PKG)/g' \
	    template.el > project-claude-generated.el

project-gemini-generated.el: template.el
	sed -e 's/@PROVIDER@/gemini/g' \
	    -e 's/@PROVIDER_TITLE@/Gemini CLI/g' \
	    -e 's/@TEMU@/$(TEMU_PKG)/g' \
	    template.el > project-gemini-generated.el

test-project-claude-generated.el: test-template.el
	sed -e 's/@PROVIDER@/claude/g' \
	    -e 's/@TEMU@/$(TEMU_PKG)/g' \
	    test-template.el > test-project-claude-generated.el

test-project-gemini-generated.el: test-template.el
	sed -e 's/@PROVIDER@/gemini/g' \
	    -e 's/@TEMU@/$(TEMU_PKG)/g' \
	    test-template.el > test-project-gemini-generated.el

.PHONY: vterm ghostty
vterm ghostty:
	rm -f $(ELGEN) $(TESTGEN)
	$(MAKE) TEMU=$@ compile

.PHONY: test
test: compile
	$(EMACS) --batch --eval "(setq package-user-dir (expand-file-name \"deps\"))" \
	  -f package-initialize \
	  -L . $(patsubst %.el,-l %,$(notdir $(TESTSRC))) \
	  -f ert-run-tests-batch-and-exit

.PHONY: dist-clean
dist-clean:
	( \
	set -e; \
	PKG_NAME=`$(EMACS) -batch -L . -l project-claude-package --eval "(princ (project-claude-package-name))"`; \
	rm -rf $${PKG_NAME}; \
	rm -rf $${PKG_NAME}.tar; \
	)

.PHONY: dist
dist: dist-clean $(ELGEN)
	$(EMACS) -batch -L . -l project-claude-package -f project-claude-package-inception
	( \
	set -e; \
	PKG_NAME=`$(EMACS) -batch -L . -l project-claude-package --eval "(princ (project-claude-package-name))"`; \
	rsync -R $(ELSRC) $(ELGEN) $${PKG_NAME} && \
	tar cf $${PKG_NAME}.tar $${PKG_NAME}; \
	)

define install-recipe
	$(MAKE) dist
	( \
	set -e; \
	INSTALL_PATH=$(1); \
	if [[ "$${INSTALL_PATH}" == /* ]]; then INSTALL_PATH=\"$${INSTALL_PATH}\"; fi; \
	PKG_NAME=`$(EMACS) -batch -L . -l project-claude-package --eval "(princ (project-claude-package-name))"`; \
	$(EMACS) --batch -l package --eval "(setq package-user-dir (expand-file-name $${INSTALL_PATH}))" \
	  -f package-initialize \
	  --eval "(ignore-errors (apply (function package-delete) (alist-get (quote project-claude) package-alist)))" \
	  -f package-refresh-contents \
	  --eval "(package-install-file \"$${PKG_NAME}.tar\")"; \
	PKG_DIR=`$(EMACS) -batch -l package --eval "(setq package-user-dir (expand-file-name $${INSTALL_PATH}))" -f package-initialize --eval "(princ (package-desc-dir (car (alist-get 'project-claude package-alist))))"`; \
	)
	$(MAKE) dist-clean
endef

$(TEMU_EL):
	git clone --depth 1 $(TEMU_REPO)

$(TEMU_SO): $(TEMU_EL)
	$(MAKE) -C $(TEMU_DIR) $(notdir $(TEMU_SO))

deps/.$(TEMU_PKG)-installed: $(TEMU_SO)
	rm -rf deps
	$(MAKE) -C $(TEMU_DIR) INSTALLDIR=$(CURDIR)/deps install
	@touch $@

deps/archives/gnu/archive-contents: deps/.$(TEMU_PKG)-installed $(ELGEN)
	$(call install-recipe,$(CURDIR)/deps)
	rm -rf deps/project-claude* # just keep deps

.PHONY: clean
clean: dist-clean
	git clean -dffX # ff because $(TEMU_DIR) has a git subdir

.PHONY: install-emacs-$(TEMU)
install-emacs-$(TEMU): $(TEMU_EL)
	$(MAKE) -C $(TEMU_DIR) INSTALLDIR=$(INSTALLDIR) install

.PHONY: install
install:
	( \
	set -e; \
	INSTALL_PATH=$(INSTALLDIR); \
	if [[ "$${INSTALL_PATH}" == /* ]]; then INSTALL_PATH=\"$${INSTALL_PATH}\"; fi; \
	1>/dev/null 2>/dev/null $(EMACS) --batch \
	  --eval "(setq package-user-dir (expand-file-name $${INSTALL_PATH}))" \
	  -f package-initialize -l $(TEMU_PKG) \
	  --eval "(or (version-list-<= '(0 0 4) \
	   (package-desc-version (car (alist-get '$(TEMU_PKG) package-alist)))) \
	   (error))" || $(MAKE) INSTALLDIR=$(INSTALLDIR) install-emacs-$(TEMU) \
	)
	$(call install-recipe,$(INSTALLDIR))
