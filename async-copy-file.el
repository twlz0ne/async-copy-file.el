;;; async-copy-file.el --- Async copy file -*- lexical-binding: t; -*-

;; Copyright (C) 2019 Gong Qijian <gongqijian@gmail.com>

;; Author: Gong Qijian <gongqijian@gmail.com>
;; Created: 2019/07/09
;; Version: 0.1.0
;; Package-Requires: ((emacs "24.4"))
;; URL: https://github.com/twlz0ne/async-copy-file.el
;; Keywords: tools, files, comm

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <https://www.gnu.org/licenses/>.

;;; Commentary:

;; Copy files asynchronously in Emacs
;;
;; ,---
;; | (async-copy-file url-or-local-file
;; |                  output-dir
;; |                  ;; following are optional
;; |                  :overwrite t
;; |                  :extract-arg '(unzip :strip-components 1)
;; |                  ;; :dry-run t
;; |                  :finish-fn
;; |                  (lambda (output-buffer)
;; |                    (kill-buffer output-buffer)
;; |                    (message "==> Finish callback")))
;; `---
;;
;; See README for more information.

;;; Change Log:

;;  0.1.0  2019/07/09  Initial version.

;;; Code:

(require 'cl-lib)

(defcustom async-copy-file-output-buffer-name "*async-copy-file*"
  "Name of output buffer."
  :type 'string
  :group 'async-copy-file)

(defvar async-copy-file--output-buffer-list '()
  "Output buffer list.")

(defun async-copy-file--get-buffer-create ()
  "Return an existing output buffer or creating a new one if needed."
  (or (catch 'found
        (mapc (lambda (buffer)
                (unless (get-buffer-process buffer)
                  (throw 'found buffer)))
              async-copy-file--output-buffer-list)
        nil)
      (let ((new-buffer (generate-new-buffer async-copy-file-output-buffer-name)))
        (push new-buffer async-copy-file--output-buffer-list)
        new-buffer)))

;;; Commands

(defun async-copy-file--copy-commands (from &optional plist to)
  (let ((path-from (if (consp from) (cdr from) from))
        (cmd-list (if (consp from) (car from) '()))
        (to (or to (make-temp-file "temp-dir--" t ".copy/"))))
    (cons (push
           (cond ((executable-find "cp")
                  (format "mkdir -p %s && cp -R %s %s" to
                          path-from
                          to))
                 (t (error "No copy tool found!")))
           cmd-list)
          to)))

(defun async-copy-file--download-commands (from &optional _plist to)
  (let ((path-from (if (consp from) (cdr from) from))
        (cmd-list (if (consp from) (car from) '()))
        (to (or to (make-temp-file "temp-file--" nil ".download.zip"))))
    (cons (push
           (cond ((executable-find "curl")
                  (format "curl -L -o %s %s" to path-from))
                 (t (error "No download tool found!")))
           cmd-list)
          to)))

(defun async-copy-file--unzip-commands (from &optional plist to)
  (let ((path-from (if (consp from) (cdr from) from))
        (cmd-list (if (consp from) (car from) '()))
        (to (or to (make-temp-file "temp-dir--" t ".extract/")))
        (strip (plist-get plist :strip-components)))
    (cons (push
           (cond
            ((executable-find "unzip")
             (format "mkdir -p %s && unzip %s -d %s" to path-from to))
            (t (error "No extract tool found!")))
           cmd-list)
          (concat to
                  (if (and strip (> strip 0))
                      (apply 'concat (make-list (1+ strip) "/*")))))))

(defun async-copy-file--tar-commands (from &optional plist to)
  (let ((path-from (if (consp from) (cdr from) from))
        (cmd-list (if (consp from) (car from) '()))
        (to (or to (make-temp-file "temp-dir--" t ".extract/")))
        (strip (plist-get plist :strip-components)))
    (cons (push
           (cond
            ((executable-find "tar")
             (format "mkdir -p %s && tar -xf %s -C %s --strip-components=%s"
                     to
                     path-from
                     to
                     (if strip strip 0)))
            (t (error "No extract tool found!")))
           cmd-list)
          to)))

;;;###autoload
(cl-defun async-copy-file-quiet (from to &key overwrite extract-arg finish-fn dry-run &allow-other-keys)
  "Same as `async-copy-file' but without output window."
  (interactive)
  (let ((display-buffer-alist
         (list
          (cons
           (concat (regexp-quote async-copy-file-output-buffer-name) ".*")
           (cons #'display-buffer-no-window nil)))))
    (async-copy-file from to
                     :overwrite   overwrite
                     :extract-arg extract-arg
                     :finish-fn   finish-fn
                     :dry-run     dry-run)))

;;;###autoload
(cl-defun async-copy-file (from to &key overwrite extract-arg finish-fn dry-run &allow-other-keys)
  "Copy file FROM to TO.
FROM can be a local file path or an url.
If :OVERWRITE is non-nil, overwrite the existing file at TO.
:EXTRACT-ARG specifies how to treat the file FROM, nil to do nothing,
or `unzip'/`tar' to extract the file. Both `unzip' and `tar' support
optional paramemter `:strip-compoments', for example:

        :extract-arg 'unzip
        ;; Equals =>
        ;; :extract-arg '(unzip :strip-components 0)
        :extract-arg '(unzip :strip-components 1)

:FINISH-FN let user to do something after process finished.
If :DRY-RUN not nil, print the final commands instead of executing."
  (let* ((final-commands nil)
         (url? (string-match-p "^https?://" from))
         (call-chain from))

    (when (and (not overwrite) (file-exists-p to))
      (signal 'file-already-exists (list "File or directory already exists" to)))

    (when url?
      (setq call-chain `(async-copy-file--download-commands ,call-chain '())))

    (when extract-arg
      (pcase (if (listp extract-arg) extract-arg (list extract-arg))
        (`(unzip . ,options)
         (setq call-chain `(async-copy-file--unzip-commands ,call-chain ',options))
         (setq call-chain `(async-copy-file--copy-commands ,call-chain '())))
        (`(tar . ,options)
         (setq call-chain `(async-copy-file--tar-commands ,call-chain ',options)))
        (_ (error (format "Unknow extract-arg: '%s'!" extract-arg)))))

    (unless (or url? extract-arg)
      (setq call-chain `(async-copy-file--copy-commands ,call-chain '())))

    (setq call-chain (append call-chain (list to)))
    (setq final-commands (mapconcat 'identity (reverse (car (eval call-chain))) " && "))

    (if dry-run
        final-commands
      (let* ((output-buffer (async-copy-file--get-buffer-create))
             (proc (progn
                     (async-shell-command final-commands output-buffer)
                     (get-buffer-process output-buffer))))
        (when (process-live-p proc)
          (set-process-sentinel
           proc
           `(lambda (proc signal)
              (when (memq (process-status proc) '(exit signal))
                (shell-command-sentinel proc signal)
                (when ',finish-fn
                  (funcall #',finish-fn ,output-buffer))))))))))

(provide 'async-copy-file)

;;; async-copy-file.el ends here
