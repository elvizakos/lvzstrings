# lvzstrings-mode #

[![License](https://img.shields.io/:license-gpl3-blue.svg)](./COPYING)

lvzstrings-mode is a minor mode for working with text on Emacs.

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
                - [Encode string for use in HTML code](#encode-string-for-use-in-html-code)
                - [Decoding HTML entities](#decoding-html-entities)
            - [Encode/Decode URL](#encodedecode-url)
                - [Encoding strings for use in URLs](#encoding-strings-for-use-in-urls)
                - [Extracting original URL from googles result URLs](#extracting-original-url-from-googles-result-urls)
            - [Encode/Decode Base64](#encodedecode-base64)
                - [About BASE64](#about-base64)
                - [Encode strings to BASE64](#encode-strings-to-base64)
                - [Decode BASE64 encoded strings](#decode-base64-encoded-strings)
            - [Remove spaces](#remove-spaces)
                - [Trim, trim left and trim right spaces](#trim-trim-left-and-trim-right-spaces)
                - [Remove multiple space characters](#remove-multiple-space-characters)
                - [Toggle the whitespace-mode](#toggle-the-whitespace-mode)
            - [Moving lines](#moving-lines)
                - [Moving lines up/down](#moving-lines-updown)
                - [Indenting / Unindenting](#indenting--unindenting)
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
This can also be installed by adding somewhere in Emacs init file the following line:
```lisp
(load "/path/to/lvzstrings-mode.el")
```

## Usage ##

### Examples ###

#### Encode/Decode HTML ####

##### Encode string for use in HTML code #####

Selecting the text `To make a part of text bold, must put this text between &lt;b&gt;&lt;/b&gt;` and pressing <kbd>C-c C-v h e</kbd> will result to:

```html
To make a part of text bold, must put this text between &lt;b&gt;&lt;/b&gt;
```

##### Decoding HTML entities #####

Selecting the text
`&amp;lt;b&amp;gt;Bold text&amp;lt;/b&amp;gt;, &amp;lt;i&amp;gt;italic text&amp;lt;/i&amp;gt;`
and pressing <kbd>C-c C-v h d</kbd> will result to:

```html
<b>Bold text</b>, <i>italic text</i>
```

#### Encode/Decode URL ####

##### Encoding strings for use in URLs #####

To encode a string that contains restricted characters, one can select that string and
use the <kbd>C-c C-v u e</kbd> key combination.

For example the string:
`https://www.youtube.com/watch?v=XmttZ-BnwaI`

will become:
`https%3A%2F%2Fwww.youtube.com%2Fwatch%3Fv%3DXmttZ-BnwaI`
and it can be used in path of a URL or as a value in a URL argument.

##### Extracting original URL from googles result URLs #####

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
10. <kbd>C-SPC</kbd>
11. <kbd>C-a</kbd>
12. <kbd>C-c C-v u d</kbd>

it will add a new line containing the extracted URL:
`https://www.youtube.com/watch?v=XmttZ-BnwaI`

#### Encode/Decode Base64 ####

##### About BASE64 #####

The characters used by BASE64 are the numbers ( `0123456789` ), the upper and lower case characters of the Latin alphabet
( `ABCDeFGHIJKLMNOPQRSTUVWXYZ` and `abcdefghijklmnopqrstuvwxyz` ) and the characters plus sing (`+`) and slash character
(`/`). The equal sign character ( `=` ) is also used as a special character.

##### Encode strings to BASE64 #####

<b>[WARNING]: encoding to base64 is not an encryption, and it shouldn't be used as such.</b>
<b>[WARNING]: Because base64 uses the characters `+`, `/` and `=`, it's not safe to use
it for parsing data in URLs. A BASE64 encoded string must also URL encoded.</b>

To encode a string to Base64, select it and press <kbd>C-c C-v b e</kbd>.

For example the following URL:

`https://www.youtube.com/watch?v=UZjuzPU9UE4`

will become:

`aHR0cHM6Ly93d3cueW91dHViZS5jb20vd2F0Y2g/dj1VWmp1elBVOVVFNA==`

##### Decode BASE64 encoded strings #####

To decode a Base64 string, select it and press <kbd>C-c C-v b d</kbd>.

For example the following:

`aHR0cHM6Ly93d3cueW91dHViZS5jb20vd2F0Y2g/dj1sZ1NMejVGZVhVZw==`

will become:

`https://www.youtube.com/watch?v=lgSLz5FeXUg`

#### Remove spaces ####

##### Trim, trim left and trim right spaces #####

By selecting a region and hitting the keys <kbd>C-c C-v t l</kbd>, it
will trim spaces on the beginning of each line in the region.

Hitting the keys <kbd>C-c C-v t r</kbd>, it will trim spaces on the
end of each line in the region.

Hitting the keys <kbd>C-c C-v t t</kbd>, it will trim spaces on the
beginning and end of each line in the region.

##### Remove multiple space characters #####

Hitting the keys <kbd>C-c C-v SPC</kbd>, it will delete multiple
continued spaces letting only one.

##### Toggle the whitespace-mode  #####

Hitting the keys <kbd>C-c C-v C-SPC</kbd>, it will toggle the
whitespace-mode on/off.

#### Moving lines ####

##### Moving lines up/down #####

Holding down the Meta and Super keys and pressing the Up or Down arrow
keys, it will move the current line, or if the region is active, the
lines in the region Up or Down in document.

##### Indenting / Unindenting #####

Holding down the Meta and Super keys and pressing the Right or Left
arrow keys, it will indent or unindent the current line, or if the
region is active, the lines in the region.

## Shortcuts ##

### Encoding/Decoding ###

| Key                    | Command / Function      | Universal argument | Description                                                                                 |
|------------------------|-------------------------|--------------------|---------------------------------------------------------------------------------------------|
| <kbd>C-c C-v h e</kbd> | `lvzstrings/htmlencode` |                    | Encodes selection for use in HTML code. For example, the "&lt;" will become "&amp;lt;".     |
| <kbd>C-c C-v h d</kbd> | `lvzstrings/htmldecode` |                    | Decodes selection from HTML encoded characters. Example: the "&amp;lt;" will become "&lt;". |
| <kbd>C-c C-v u e</kbd> | `lvzstrings/urlencode`  |                    | Encodes selection for use in a URL. For example, the "=" will become "%3D".                 |
| <kbd>C-c C-v u d</kbd> | `lvzstrings/urldecode`  |                    | Decodes selected from URL encoded characters. For example the "%3D" will become "=".        |
| <kbd>C-c C-v b e</kbd> | `base64-encode-region`  |                    |                                                                                             |
| <kbd>C-c C-v b d</kbd> | `base64-decode-region`  |                    |                                                                                             |

### Trimming strings (removing extra spaces) ###

| Key                      | Command / Function    | Universal argument | Description                                                                 |
|--------------------------|-----------------------|--------------------|-----------------------------------------------------------------------------|
| <kbd>C-c C-v t l</kbd>   | `lvzstrings/ltrim`    |                    | Remove the spaces at the beginning (left) of the selection.                 |
| <kbd>C-c C-v t r</kbd>   | `lvzstrings/rtrim`    |                    | Remove the spaces at the end (right) of the selection.                      |
| <kbd>C-c C-v t t</kbd>   | `lvzstrings/trim`     |                    | Remove the spaces at the beginning (left) and end (right) of the selection. |
| <kbd>C-c C-v SPC</kbd>   | `lvzstrings/onespace` |                    | Where there are multiple spaces remove them and leave only one.             |
| <kbd>C-c C-v C-SPC</kbd> | `whitespace-mode`     |                    | Toggle whitespace-mode on/off.                                              |

### Move lines ###

| Key                          | Command / Function          | Universal argument | Description                                            |
|------------------------------|-----------------------------|--------------------|--------------------------------------------------------|
| <kbd>&lt;M-s-up&gt;</kbd>    | `lvzstrings/move-line-up`   |                    | Move up current line or lines in selection.            |
| <kbd>&lt;M-s-down&gt;</kbd>  | `lvzstrings/move-line-down` |                    | Move down current line or lines in selection.          |
| <kbd>&lt;M-s-right&gt;</kbd> | `lvzstrings/indent`         |                    | Increase indent of current line or lines in selection. |
| <kbd>&lt;M-s-left&gt;</kbd>  | `lvzstrings/unindent`       |                    | Decrease indent of current line of lines in selection. |
