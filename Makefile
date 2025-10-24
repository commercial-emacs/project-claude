SHELL := /bin/bash
EMACS ?= emacs
ELSRC := $(shell git ls-files project*.el)
TESTSRC := $(shell git ls-files test-project*.el)
ELGEN := project-claude-generated.el project-gemini-generated.el
TESTGEN := test-project-claude-generated.el test-project-gemini-generated.el
INSTALLDIR ?= package-user-dir
.PHONY: compile
compile: deps/archives/gnu/archive-contents $(ELGEN) $(TESTGEN)
	$(EMACS) -batch \
	  --eval "(setq byte-compile-error-on-warn t)" \
	  --eval "(setq package-user-dir (expand-file-name \"deps\"))" \
	  -f package-initialize \
	  -L . \
	  -f batch-byte-compile $(ELSRC) $(TESTSRC); \
	  (ret=$$? ; rm -f $(ELSRC:.el=.elc) $(TESTSRC:.el=.elc) && exit $$ret)

project-claude-generated.el: template.el
	sed -e 's/@PROVIDER@/claude/g' \
	    -e 's/@PROVIDER_TITLE@/Claude Code/g' \
	    template.el > project-claude-generated.el

project-gemini-generated.el: template.el
	sed -e 's/@PROVIDER@/gemini/g' \
	    -e 's/@PROVIDER_TITLE@/Gemini CLI/g' \
	    template.el > project-gemini-generated.el

test-project-claude-generated.el: test-template.el
	sed -e 's/@PROVIDER@/claude/g' \
	    test-template.el > test-project-claude-generated.el

test-project-gemini-generated.el: test-template.el
	sed -e 's/@PROVIDER@/gemini/g' \
	    test-template.el > test-project-gemini-generated.el

.PHONY: test
test: compile
	$(EMACS) --batch --eval "(setq package-user-dir (expand-file-name \"deps\"))" \
	  -f package-initialize \
	  -L . $(patsubst %.el,-l %,$(notdir $(TESTSRC))) \
	  -f ert-run-tests-batch

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

emacs-libvterm/vterm.el:
	git clone --depth 1 https://github.com/commercial-emacs/emacs-libvterm.git

emacs-libvterm/vterm-module.so: emacs-libvterm/vterm.el
	rm -rf deps
	$(MAKE) -C emacs-libvterm INSTALLDIR=$(CURDIR)/deps install

deps/archives/gnu/archive-contents: emacs-libvterm/vterm-module.so
	$(call install-recipe,$(CURDIR)/deps)
	rm -rf deps/project-claude* # just keep deps

.PHONY: clean
clean: dist-clean
	git clean -dffX # ff because emacs-libvterm has a git subdir

.PHONY: install-emacs-libvterm
install-emacs-libvterm: emacs-libvterm/vterm.el
	$(MAKE) -C emacs-libvterm INSTALLDIR=$(INSTALLDIR) install

.PHONY: install
install:
	( \
	set -e; \
	INSTALL_PATH=$(INSTALLDIR); \
	if [[ "$${INSTALL_PATH}" == /* ]]; then INSTALL_PATH=\"$${INSTALL_PATH}\"; fi; \
	1>/dev/null 2>/dev/null $(EMACS) --batch \
	  --eval "(setq package-user-dir (expand-file-name $${INSTALL_PATH}))" \
	  -f package-initialize -l vterm \
	  --eval "(or (version-list-<= '(0 0 4) \
	   (package-desc-version (car (alist-get 'vterm package-alist)))) \
	   (error))" || $(MAKE) INSTALLDIR=$(INSTALLDIR) install-emacs-libvterm \
	)
	$(call install-recipe,$(INSTALLDIR))
