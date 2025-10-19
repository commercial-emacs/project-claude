SHELL := /bin/bash
EMACS ?= emacs
ELSRC := $(shell git ls-files project*.el)
TESTSRC := $(shell git ls-files test-*.el)

project-claude-generated.el: template.el
	sed -e 's/@PROVIDER@/claude/g' \
	    -e 's/@PROVIDER_TITLE@/Claude/g' \
	    -e 's/@CLEAR_INPUT_REGEX@/\\s-+──/g' \
	    template.el > project-claude-generated.el

project-gemini-generated.el: template.el
	sed -e 's/@PROVIDER@/gemini/g' \
	    -e 's/@PROVIDER_TITLE@/Gemini/g' \
	    -e 's/@CLEAR_INPUT_REGEX@/\\s-+│/g' \
	    template.el > project-gemini-generated.el

.PHONY: compile
compile: deps/archives/gnu/archive-contents project-claude-generated.el project-gemini-generated.el
	$(EMACS) -batch \
	  --eval "(setq byte-compile-error-on-warn t)" \
	  --eval "(setq package-user-dir (expand-file-name \"deps\"))" \
	  -f package-initialize \
	  -L . \
	  -f batch-byte-compile $(ELSRC) $(TESTSRC); \
	  (ret=$$? ; rm -f $(ELSRC:.el=.elc) $(TESTSRC:.el=.elc) && exit $$ret)

.PHONY: test
test: compile
	$(EMACS) --batch --eval "(setq package-user-dir (expand-file-name \"deps\"))" \
	  -f package-initialize \
	  -L . $(patsubst %.el,-l %,$(notdir $(TESTSRC))) \
	  -f ert-run-tests-batch

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
	rsync -R $(ELSRC) $${PKG_NAME} && \
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

emacs-libvterm/vterm.el:
	git clone --depth 1 https://github.com/commercial-emacs/emacs-libvterm.git

# I'd use a vterm-specific filename but I don't want to hardcode version
emacs-libvterm/deps/archives/gnu/archive-contents:
	rm -rf deps
	$(MAKE) emacs-libvterm/vterm.el
	$(MAKE) -C emacs-libvterm deps/archives/gnu/archive-contents
	mkdir -p deps
	cp -pr emacs-libvterm/deps/vterm* deps/

deps/archives/gnu/archive-contents: emacs-libvterm/deps/archives/gnu/archive-contents
	$(call install-recipe,\"deps\")
	rm -rf deps/project-claude* # just keep deps

.PHONY: clean
clean:
	git clean -dffx # ff because emacs-libvterm has a git subdir

.PHONY: install-emacs-libvterm
install-emacs-libvterm:
	$(MAKE) emacs-libvterm/vterm.el
	$(MAKE) -C emacs-libvterm install

.PHONY: install
install:
	@$(EMACS) --batch -f package-initialize -l vterm \
	  --eval "(or (version-list-<= '(0 0 4) \
	   (package-desc-version (car (alist-get 'vterm package-alist)))) \
	   (error))" || $(MAKE) install-emacs-libvterm
	$(call install-recipe,package-user-dir)
