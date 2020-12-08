
# bmx-mode

![CI](https://github.com/josteink/bmx-mode/workflows/CI/badge.svg)

![logo](https://raw.githubusercontent.com/josteink/bmx-mode/master/logo.png)

Action-paced Windows Batch-file editing, powered by [GNU Emacs](https://www.gnu.org/software/emacs/)!

Batch Mode eXtras, or bmx for short is an Emacs-package which aims to
extend Emacs' built in `bat-mode`, to improve editing of Windows
Batch-files.

## Features

The following features are currently provided:

* Code-completion for labels and environment-variables based on [company-mode](https://company-mode.github.io/) (`C-.`).
* Seamless code-navigation using established Emacs-conventions (`M-,` and `M-.`).
* Looking up references to any symbol (`Shift-f12`).
* Renaming of code-symbols (`C-c C-r`).
* Fixup file to use consistent casing and syntax for all variables and labels (`C-c C-f`).


## Installation

`bmx-mode` is packaged and available from MELPA. Install it the usual way:

`M-x package-install<RET>bmx-mode<RET>`

## Configuration/Customization

Install it and plug it into your `.emacs` and you should be done:

``` elisp
(require 'bmx-mode)
(bmx-mode-setup-defaults)
```
or use `use-package`:

``` elisp
(use-package bmx-mode
    :config (bmx-mode-setup-defaults))
```

No further configuration should be needed to work properly.

That said the following aspects can be customized using standard Emacs-customization (`M-x customize-group<RET>bmx-mode`).

* Toggle completion of system-variables (default `off`)

## Status

While fully usable, this mode is currently under development and there may be bugs.

If so feel free to report them, or even better: Provide PRs!

