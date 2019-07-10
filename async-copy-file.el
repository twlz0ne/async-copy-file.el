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

;; 

;;; Change Log:

;;  0.1.0  2019/07/09  Initial version.

;;; Code:

(require 'cl-lib)

(defun async-copy-file--copy-commands (from &optional to plist)
  (let ((path-from (if (consp from) (cdr from) from))
        (cmd-list (if (consp from) (car from) '()))
        (to (or to (make-temp-file "temp-dir--" t ".copy/")))
        (strip (plist-get plist :strip-components)))
    (cons (push
           (cond ((executable-find "cp")
                  (format "mkdir -p %s && cp -R %s %s" to
                          (concat path-from
                                  (if (and strip (> strip 0))
                                      (apply 'concat (make-list (1+ strip) "/*"))))
                          to))
                 (t (error "No copy tool found!")))
           cmd-list)
          to)))

(defun async-copy-file--download-commands (from &optional to _plist)
  (let ((path-from (if (consp from) (cdr from) from))
        (cmd-list (if (consp from) (car from) '()))
        (to (or to (make-temp-file "temp-file--" nil ".download.zip"))))
    (cons (push
           (cond ((executable-find "curl")
                  (format "curl -L -o %s %s" to path-from))
                 (t (error "No download tool found!")))
           cmd-list)
          to)))

(defun async-copy-file--unzip-commands (from &optional to _plist)
  (let ((path-from (if (consp from) (cdr from) from))
        (cmd-list (if (consp from) (car from) '()))
        (to (or to (make-temp-file "temp-dir--" t ".extract/"))))
    (cons (push
           (cond
            ((executable-find "unzip")
             (format "mkdir -p %s && unzip %s -d %s" to path-from to))
            (t (error "No extract tool found!")))
           cmd-list)
          to)))

(cl-defun async-copy-file (from to &key overwrite extracter strip-components finish-fn dry-run &allow-other-keys)
  (let* ((final-commands nil)
         (url? (string-match-p "^https?://" from))
         (call-chain from))

    (when url?
      (setq call-chain (list 'async-copy-file--download-commands call-chain)))

    (when extracter
      (cond
       ((eq extracter 'unzip) (setq call-chain (list 'async-copy-file--unzip-commands call-chain)))
       (t (error (format "Unknow extracter '%s'!" extracter)))))

    (when (or (not url?) extracter)
      (setq call-chain (list 'async-copy-file--copy-commands call-chain)))

    (setq call-chain (append call-chain (list to `(list :strip-components ,strip-components))))
    (setq final-commands (mapconcat 'identity (reverse (car (eval call-chain))) " && "))

    (if dry-run
        final-commands
      (let* ((output-buffer (generate-new-buffer "*async-copy-file*"))
             (proc (progn
                     (async-shell-command final-commands output-buffer)
                     (get-buffer-process output-buffer))))
        (when (process-live-p proc)
          (message "==> Start proc")
          (set-process-sentinel
           proc
           `(lambda (proc signal)
              (when (memq (process-status proc) '(exit signal))
                (shell-command-sentinel proc signal)
                (message "==> Finished")
                (funcall #',finish-fn ,output-buffer)))))))))

(provide 'async-copy-file)

;;; async-copy-file.el ends here
