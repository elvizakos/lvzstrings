# lvzstrings-mode #

[![License](https://img.shields.io/:license-gpl3-blue.svg)](./COPYING)

lvzstrings-mode is a minor mode for working with text on emacs.

<!-- markdown-toc start - Don't edit this section. Run M-x markdown-toc-refresh-toc -->
**Table of Contents**

- [lvzstrings-mode](#lvzstrings-mode)
    - [Installation](#installation)
        - [Build package](#build-package)
        - [Installing package](#installing-package)
        - [Simple installation](#simple-installation)
    - [Usage](#usage)
        - [Examples](#examples)
            - [Encode/Decode HTML](#encodedecode-html)
            - [Encode/Decode URL](#encodedecode-url)
            - [Encode/Decode Base64](#encodedecode-base64)
            - [Remove extra spaces](#remove-extra-spaces)
        - [Shortcuts](#shortcuts)
            - [Encoding/Decoding](#encodingdecoding)
            - [Trimming strings (removing extra spaces)](#trimming-strings-removing-extra-spaces)
            - [Move lines](#move-lines)

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

If there is a previous version of lvzstrings first execute:
```bash
make remove
```

If there is no previous version or if it has been removed, execute:
```bash

make install
```

### Simple installation ###
This can also be installed by adding somewhere in emacs init file the following line:
```lisp
(load "/path/to/lvzstrings-mode.el")
```

## Usage ##

### Examples ###

#### Encode/Decode HTML ####

Encode string for use in HTML code:

Selecting the text <kbd>To make a part of text bold, must put this text between &lt;b&gt;&lt;/b&gt;</kbd> and pressing <kbd>C-c C-v h e</kbd> will result to:

```html
To make a part of text bold, must put this text between &lt;b&gt;&lt;/b&gt;
```

Decoding html entities:

Selecting the text
`&amp;lt;b&amp;gt;Bold text&amp;lt;/b&amp;gt;, &amp;lt;i&amp;gt;italic text&amp;lt;/i&amp;gt;`
and pressing <kbd>C-c C-v h d</kbd> will result to:

```html
<b>Bold text</b>, <i>italic text</i>
```

#### Encode/Decode URL ####

Encoding strings for use in URLs:

To encode a string that contains restricted characters, one can select that string and
use the <kbd>C-c C-v u e</kbd> key combination.

For example the string:
`https://www.youtube.com/watch?v=XmttZ-BnwaI`

will become:
`https%3A%2F%2Fwww.youtube.com%2Fwatch%3Fv%3DXmttZ-BnwaI`

Extracting original URL from googles result URLs:

On the following URL:
`https://www.google.com/url?sa=t&rct=j&q=&esrc=s&source=web&cd=&cad=rja&uact=8&ved=2ahUKEwiH1-PEoIP-AhUF3aQKHcZkD_kQwqsBegQIChAB&url=https%3A%2F%2Fwww.youtube.com%2Fwatch%3Fv%3DXmttZ-BnwaI&usg=AOvVaw1Mzp0sb9VJ4xFQiZ60vID9`

using this sequence

1. <kbd>C-s &url=</kbd>
2. <kbd>C-SPC</kbd>
3. <kbd>C-s &</kbd>
4. <kbd>C-b</kbd>
5. <kbd>M-w</kbd>
7. <kbd>C-a</kbd>
8. <kbd>C-o</kbd>
9. <kbd>C-y</kbd>
10.<kbd>C-SPC</kbd>
11.<kbd>C-a</kbd>
12.<kbd>C-c C-v u d</kbd>

it will add a new line containing the extracted URL:
`https://www.youtube.com/watch?v=XmttZ-BnwaI`

#### Encode/Decode Base64 ####

<kbd>C-c C-v b e</kbd>
<kbd>C-c C-v b d</kbd>

#### Remove extra spaces ####

<kbd>C-c C-v t l</kbd>
<kbd>C-c C-v t r</kbd>
<kbd>C-c C-v t t</kbd>
<kbd>C-c C-v SPC</kbd>

Selecting text 


```
;; this is 	a    string
```

```
;; this is 	a string
```
Notice that after "is" there is a space character and before "a" there is a tab character.

### Shortcuts ###

#### Encoding/Decoding ####

| Key                    | Command / Function      | Universal argument | Description                                                                                 |
|------------------------|-------------------------|--------------------|---------------------------------------------------------------------------------------------|
| <kbd>C-c C-v h e</kbd> | `lvzstrings/htmlencode` |                    | Encodes selection for use in HTML code. For example, the "&lt;" will become  "&amp;lt;".    |
| <kbd>C-c C-v h d</kbd> | `lvzstrings/htmldecode` |                    | Decodes selection from HTML encoded characters. Example: the "&amp;lt;" will become "&lt;". |
| <kbd>C-c C-v u e</kbd> | `lvzstrings/urlencode`  |                    | Encodes selection for use in a URL. For example, the "=" will become "%3D".                 |
| <kbd>C-c C-v u d</kbd> | `lvzstrings/urldecode`  |                    | Decodes selected from URL encoded characters. For example the "%3D" will becode "=".        |
| <kbd>C-c C-v b e</kbd> | `base64-encode-region`  |                    |                                                                                             |
| <kbd>C-c C-v b d</kbd> | `base64-decode-region`  |                    |                                                                                             |

#### Trimming strings (removing extra spaces) ####

| Key                      | Command / Function    | Universal argument | Description                                                                 |
|--------------------------|-----------------------|--------------------|-----------------------------------------------------------------------------|
| <kbd>C-c C-v t l</kbd>   | `lvzstrings/ltrim`    |                    | Remove the spaces at the beginning (left) of the selection.                 |
| <kbd>C-c C-v t r</kbd>   | `lvzstrings/rtrim`    |                    | Remove the spaces at the end (right ) of the selection.                     |
| <kbd>C-c C-v t t</kbd>   | `lvzstrings/trim`     |                    | Remove the spaces at the beginning (left) and end (right) of the selection. |
| <kbd>C-c C-v SPC</kbd>   | `lvzstrings/onespace` |                    | Where there are multiple spaces remove them and leave only one.             |
| <kbd>C-c C-v C-SPC</kbd> | `whitespace-mode`     |                    | Toggle whilespace-mode on/off.                                              |

#### Move lines ####

| Key                          | Command / Function          | Univerasl argument | Description                                            |
|------------------------------|-----------------------------|--------------------|--------------------------------------------------------|
| <kbd>&lt;M-s-up&gt;</kbd>    | `lvzstrings/move-line-up`   |                    | Move up currect line or lines in selection.            |
| <kbd>&lt;M-s-down&gt;</kbd>  | `lvzstrings/move-line-down` |                    | Move down current line or lines in selection.          |
| <kbd>&lt;M-s-right&gt;</kbd> | `lvzstrings/indent`         |                    | Increase indent of current line or lines in selection. |
| <kbd>&lt;M-s-left&gt;</kbd>  | `lvzstrings/unindent`       |                    | Decrease indent of current line of lines in selection. |
