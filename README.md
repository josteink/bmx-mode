
# bmx-mode

[![Build Status](https://api.travis-ci.org/josteink/bmx-mode.svg?branch=master)](https://travis-ci.org/josteink/bmx-mode)

Action-paced batch-file editing, powered by Emacs!

Batch Mode eXtras, or bmx for short is an Emacs-package which aims to
extend Emacs' built in `bat-mode`, to improve editing of Windows
BATCH-files.

## features

The following features are currently provided:

* Targeted auto-completion of `goto` and `call` labels.
* Targeted auto-completion of environment-variables.
  (System variables can be enabled through configuration/customization).
* Looking up references for symbol at point (labels & variables).
* Navigating to symbol at point (labels & variables).
* Renaming symbol at point (labels & variables).
* Fixup file to use consistent label and variable-references and syntax.

## status

While fully usable, this mode is currently under development and is
not packaged/finalized, not prepared for publishing on
package-repositories like MELPA.

