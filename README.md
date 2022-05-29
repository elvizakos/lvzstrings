# lvzstrings-mode #

[![License](https://img.shields.io/:license-gpl3-blue.svg)](./COPYING)

lvzstrings-mode is a minor mode for working with text on emacs.

<!-- markdown-toc start - Don't edit this section. Run M-x markdown-toc-generate-toc again -->
**Table of Contents**

- [lvzstrings-mode](#lvzstrings-mode)
    - [Installation](#installation)
        - [Build package](#build-package)
        - [Installing package](#installing-package)
        - [Simple installation](#simple-installation)
    - [Usage](#usage)
        - [Remove extra spaces](#remove-extra-spaces)

<!-- markdown-toc end -->

## Installation ##

### Build package ###

```bash
## Create empty file "lvzstrings-mode-autoloads"
echo "" > lvzstrings-mode-autoloads

## Clear previous package build files
make clean

## Make package
make
```

### Installing package ###

```bash

cd ..

## Install package
tar -xvf ".<TAR_FILE_PACKAGE>" -C "~/.emacs.d/elpa/"

```

### Simple installation ###
This can also be installed by adding somewhere in emacs init file the following line:
```lisp
(load "/path/to/lvzstrings-mode.el")
```

## Usage ##

### Remove extra spaces ###

Selecting text 


```
;; this is 	a    string
```

```
;; this is 	a string
```
Notice that after "is" there is a space character and before "a" there is a tab character.
