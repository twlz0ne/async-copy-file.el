[![Build Status](https://travis-ci.com/twlz0ne/async-copy-file.el.svg?branch=master)](https://travis-ci.com/twlz0ne/async-copy-file.el)

# async-copy-file.el

Copy files asynchronously in Emacs

## Installation

Clone this repository to `~/.emacs.d/site-lisp/async-copy-file`. Add the following to your `.emacs`:

```elisp
(add-to-list 'load-path (expand-file-name "~/.emacs.d/site-lisp/async-copy-file"))
(require 'async-copy-file)
```

## Usage

```elisp
(async-copy-file
 url-or-local-file
 output-dir

 ;; following are optional

 :overwrite t                             ;; Overwrite existing file

 :extract-arg '(unzip :strip-component 1) ;; Extract file using unzip (also supports tar),
                                          ;; and remove the leading path

 ;; :dry-run t                            ;; Print finale command instead of executing

 :finish-fn                               ;; Call back after process finished
 (lambda (output-buffer)
   (kill-buffer output-buffer)
   (message "==> Finish callback")))
```
