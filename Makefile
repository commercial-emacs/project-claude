SHELL := /bin/bash
EMACS ?= emacs
ELSRC := $(shell git ls-files project*.el)
TESTSRC := $(shell git ls-files test-*.el)

.PHONY: compile
compile: deps/archives/gnu/archive-contents
	$(EMACS) -batch \
	  --eval "(setq byte-compile-error-on-warn t)" \
	  --eval "(setq package-user-dir \"deps\")" \
	  -f package-initialize \
	  -L . \
	  -f batch-byte-compile $(ELSRC) $(TESTSRC); \
	  (ret=$$? ; rm -f $(ELSRC:.el=.elc) $(TESTSRC:.el=.elc) && exit $$ret)

.PHONY: test
test: compile
	$(EMACS) --batch -L . $(patsubst %.el,-l %,$(notdir $(TESTSRC))) -f ert-run-tests-batch

.PHONY: dist-clean
dist-clean:
	( \
	PKG_NAME=`$(EMACS) -batch -L . -l project-claude-package --eval "(princ (project-claude-package-name))"`; \
	rm -rf $${PKG_NAME}; \
	rm -rf $${PKG_NAME}.tar; \
	)

.PHONY: dist
dist: dist-clean
	$(EMACS) -batch -L . -l project-claude-package -f project-claude-package-inception
	( \
	PKG_NAME=`$(EMACS) -batch -L . -l project-claude-package --eval "(princ (project-claude-package-name))"`; \
	rsync -R $(ELSRC) $${PKG_NAME}; \
	tar cf $${PKG_NAME}.tar $${PKG_NAME}; \
	)

define install-recipe
	$(MAKE) dist
	( \
	PKG_NAME=`$(EMACS) -batch -L . -l project-claude-package --eval "(princ (project-claude-package-name))"`; \
	$(EMACS) --batch -l package --eval "(setq package-user-dir (expand-file-name $(1)))" \
	  -f package-initialize \
	  --eval "(ignore-errors (apply (function package-delete) (alist-get (quote project-claude) package-alist)))" \
	  -f package-refresh-contents \
	  --eval "(package-install-file \"$${PKG_NAME}.tar\")"; \
	PKG_DIR=`$(EMACS) -batch -l package --eval "(setq package-user-dir (expand-file-name $(1)))" -f package-initialize --eval "(princ (package-desc-dir (car (alist-get 'project-claude package-alist))))"`; \
	)
	$(MAKE) dist-clean
endef

emacs-libvterm/deps/archives/gnu/archive-contents:
	rm -rf emacs-libvterm
	git clone --depth 1 https://github.com/commercial-emacs/emacs-libvterm.git
	make -C emacs-libvterm deps/archives/gnu/archive-contents
	cp -pr emacs-libvterm/deps/vterm* deps/

deps/archives/gnu/archive-contents: emacs-libvterm/deps/archives/gnu/archive-contents
	$(call install-recipe,\"deps\")

.PHONY: install
install:
	$(call install-recipe,package-user-dir)
