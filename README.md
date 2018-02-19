
# bmx-mode

[![Build Status](https://api.travis-ci.org/josteink/bmx-mode.svg?branch=master)](https://travis-ci.org/josteink/bmx-mode)

![logo](https://raw.githubusercontent.com/josteink/bmx-mode/master/logo.png)

Action-paced Windows Batch-file editing, powered by [GNU Emacs](https://www.gnu.org/software/emacs/)!

Batch Mode eXtras, or bmx for short is an Emacs-package which aims to
extend Emacs' built in `bat-mode`, to improve editing of Windows
Batch-files.

## features

The following features are currently provided:

* Code-completion for labels and environment-variables based on [company-mode](https://company-mode.github.io/) (`C-.`).
* Seamless code-navigation using established Emacs-conventions (`M-,` and `M-.`).
* Looking up references to any symbol (`Shift-f12`).
* Renaming of code-symbols (`C-c C-r`).
* Fixup file to use consistent casing and syntax for all variables and labels (`C-c C-f`).


## installation

`bmx-mode` is packaged and available from MELPA. Install it the usual way:

`M-x package-install<RET>bmx-mode<RET>`

## configuration/customization

Install it and plug it into your `.emacs` and you should be done:

````elisp
(require 'bmx-mode)
(bmx-mode-setup-defaults)
````

No further configuration should be needed to work properly.

That said the following aspects can be customized using standard Emacs-customization (`M-x customize-group<RET>bmx-mode`).

* Toggle completion of system-variables (default `off`)

## status

While fully usable, this mode is currently under development and is
not packaged/finalized, not prepared for publishing on
package-repositories like MELPA.

