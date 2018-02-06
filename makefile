EMACS=$(shell which emacs) -Q -batch -L .
WORKDIR=/tmp/bmx-mode
export HOME := $(WORKDIR)

all: test build

build:
	cask build

test: build
	mkdir -p $(WORKDIR)
	+ $(EMACS) -l bmx-mode-tests.el -f ert-run-tests-batch-and-exit

clean:
	rm -rf *.elc
	rm -rf $(WORKDIR)
