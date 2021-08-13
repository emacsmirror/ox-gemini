update:
	emacs -batch -l test/make-update.el

compile: clean lint
	emacs -batch -l test/elpa.el -l test/compile.el

clean:
	rm -f *.elc

lint:
	./test/checkdoc-batch.sh -Q --lispdir=test ox-gemini.el
.PHONY: update compile test clean
