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

;;; Buffer

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
                (when (buffer-live-p buffer)
                  (unless (get-buffer-process buffer)
                    (throw 'found buffer))))
              async-copy-file--output-buffer-list)
        nil)
      (let ((new-buffer (generate-new-buffer async-copy-file-output-buffer-name)))
        (push new-buffer async-copy-file--output-buffer-list)
        new-buffer)))

;;; Progress

(declare-function spinner-start "spinner")
(declare-function lv-message "lv")
(declare-function lv-delete-window "lv")

(defcustom async-copy-file-progress-type nil
  "The way to display copy progress.

* header-line   Show header line with last content of output buffer
* lv-message    SHow lv message last with last content of output buffer
* spinner       Show spinner in mode line
* quiet         Show nothing

If it is nil, view output buffer in other window."
  :type '(choice
          (const header-line)
          (const lv-message)
          (const spinner)
          (const quiet)
          (const nil))
  :group 'async-copy-file)

(defun async-copy-file--progress-stop (output-buffer current-buffer progress)
  (pcase (car progress)
    (`,(and type (guard (memq type '(header-line lv-message))))
     (cancel-timer (cdr progress))
     (if (eq type 'lv-message)
         (lv-delete-window)
       (with-current-buffer current-buffer
         (setq-local header-line-format nil))))
    (`spinner (funcall (cdr progress)))))

(defun async-copy-file--progress-timer (output-buffer current-buffer type)
  (with-current-buffer output-buffer
    (let ((progress-line (save-excursion
                           (goto-char (point-max))
                           (re-search-backward "[:alnum:]" nil t)
                           (buffer-substring-no-properties (point-at-bol) (point-at-eol)))))
      (if (eq type 'lv-message)
          (lv-message "[async-copy-file] %s" progress-line)
        (with-current-buffer current-buffer
          (setq-local header-line-format (format "[async-copy-file] %s" progress-line)))))))

(defun async-copy-file--progress-start (output-buffer current-buffer)
  (pcase async-copy-file-progress-type
    (`,(and type (guard (memq type '(header-line lv-message))))
     (if (eq type 'lv-message)
         (lv-message "[async-copy-fie] Start...")
       (setq-local header-line-format "[async-copy-fie] Start..."))
     (let ((timer (run-with-timer 1 1
                                  'async-copy-file--progress-timer
                                  output-buffer
                                  current-buffer
                                  type)))
       (cons type timer)))
    (`spinner (cons 'spinner (spinner-start)))))

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
  (let ((async-copy-file-progress-type 'quiet))
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
         (call-chain from)
         (display-buffer-alist
          (when (memq async-copy-file-progress-type '(header-line lv-message spinner quiet))
            (list
             (cons
              (concat (regexp-quote async-copy-file-output-buffer-name) ".*")
              (cons #'display-buffer-no-window nil))))))

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
             (current-buffer (current-buffer))
             (proc (progn
                     (async-shell-command final-commands output-buffer)
                     (get-buffer-process output-buffer)))
             (progress))
        (when (process-live-p proc)
          (setq progress (async-copy-file--progress-start output-buffer current-buffer))
          (set-process-sentinel
           proc
           `(lambda (proc signal)
              (when (memq (process-status proc) '(exit signal))
                (shell-command-sentinel proc signal)
                (when ',progress
                  (async-copy-file--progress-stop ,output-buffer ,current-buffer ',progress))
                (when ',finish-fn
                  (funcall #',finish-fn ,output-buffer))))))))))

(provide 'async-copy-file)

;;; async-copy-file.el ends here
