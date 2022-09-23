SHELL = /bin/sh
EMACS ?= emacs
PROFILER =

.PHONY: test clean githooks

# org:
	# $(EMACS) --batch --eval "(progn (require 'org) (setq org-confirm-babel-evaluate nil) (org-babel-tangle-file \"org/config.org\"))"

time:
	$(EMACS) -q --eval '(message "%s" (emacs-init-time))'

# Delete byte-compiled files etc.
clean:
	@rm -f *~
	@rm -f \#*\#
	@rm -f *.elc

githooks:
	cd `git rev-parse --show-toplevel`/.git/hooks && ln -s ../../githooks/pre-commit pre-commit && cd -

# Run tests.
test: clean
	@$(EMACS) -Q -nw --batch -l init.el --eval '(progn (memory-report) (message (buffer-string)))'
	@$(EMACS) -nw --batch -l init.el --eval '(message "startup time: %s, gcs-done=%d" (emacs-init-time) gcs-done)'
# -l tests/emacs.d-test.el
