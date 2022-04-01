test:
	emacs -Q -batch -f package-initialize -L . -f buttercup-run-discover
