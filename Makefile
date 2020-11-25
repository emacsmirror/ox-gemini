update:
	emacs -batch -l test/make-update.el

compile: clean
	emacs -batch -l test/elpa.el -l test/compile.el

clean:
	rm -f *.elc

.PHONY: update compile test clean
