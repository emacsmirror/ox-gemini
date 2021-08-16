update:
	emacs -batch -l test/make-update.el

compile: clean lint
	emacs --version
	emacs -batch -l test/elpa.el -l test/compile.el

clean:
	rm -f *.elc

lint:
	emacs --batch -l test/lint.el

.PHONY: update compile test clean
