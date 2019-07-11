;;; async-copy-file-test.el --- Test async-copy-file -*- lexical-binding: t; -*-

;; Copyright (C) 2019 Gong Qijian <gongqijian@gmail.com>

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <http://www.gnu.org/licenses/>.

;;; Code:

(require 'ert)
(require 'async-copy-file)

(when noninteractive
  (transient-mark-mode))

(defvar async-copy-file-test-sample-zip-url "https://github.com/twlz0ne/async-copy-file.el/archive/v0.1.0.zip")
(defvar async-copy-file-test-sample-zip-file (expand-file-name "async-copy-file.el-0.1.0.zip"))
(defvar async-copy-file-test-sample-tar-url "https://github.com/twlz0ne/async-copy-file.el/archive/v0.1.0.zip")
(defvar async-copy-file-test-sample-tar-file (expand-file-name "async-copy-file.el-0.1.0.tar.gz"))
(defvar async-copy-file-test-output-dir "/tmp/emacs-async-copy-file-test")

(unless (file-exists-p async-copy-file-test-sample-zip-file)
  (url-copy-file async-copy-file-test-sample-zip-url async-copy-file-test-sample-zip-file))

(unless (file-exists-p async-copy-file-test-sample-tar-file)
  (url-copy-file async-copy-file-test-sample-tar-url async-copy-file-test-sample-tar-file))

(defun async-copy-file-test (from to &rest rest)
  (ignore-errors
    (delete-directory to t))
  (mkdir async-copy-file-test-output-dir t)
  (apply 'async-copy-file from to rest))

;;; local file

(ert-deftest async-copy-file-test-local-zip-file-0 ()
  (async-copy-file-test
   async-copy-file-test-sample-zip-file
   async-copy-file-test-output-dir
   :extract-arg 'unzip
   :finish-fn
   (lambda (_)
     (should
      (and (file-exists-p (concat async-copy-file-test-output-dir "/async-copy-file.el-0.1.0/README.md"))
           (file-exists-p (concat async-copy-file-test-output-dir "/async-copy-file.el-0.1.0/LICENSE")))))))

(ert-deftest async-copy-file-test-local-zip-file-1 ()
  (async-copy-file-test
   async-copy-file-test-sample-zip-file
   async-copy-file-test-output-dir
   :extract-arg 'unzip
   :finish-fn
   (lambda (_)
     (should
      (and (file-exists-p (concat async-copy-file-test-output-dir "/README.md"))
           (file-exists-p (concat async-copy-file-test-output-dir "/LICENSE")))))))

(ert-deftest async-copy-file-test-local-tar-file-0 ()
  (async-copy-file-test
   async-copy-file-test-sample-tar-file
   async-copy-file-test-output-dir
   :extract-arg 'tar
   :finish-fn
   (lambda (_)
     (should
      (and (file-exists-p (concat async-copy-file-test-output-dir "/async-copy-file.el-0.1.0/README.md"))
           (file-exists-p (concat async-copy-file-test-output-dir "/async-copy-file.el-0.1.0/LICENSE")))))))

(ert-deftest async-copy-file-test-local-tar-file-1 ()
  (async-copy-file-test
   async-copy-file-test-sample-tar-file
   async-copy-file-test-output-dir
   :extract-arg 'tar
   :finish-fn
   (lambda (_)
     (should
      (and (file-exists-p (concat async-copy-file-test-output-dir "/README.md"))
           (file-exists-p (concat async-copy-file-test-output-dir "/LICENSE")))))))

;;; url

(ert-deftest async-copy-file-test-url-zip-0 ()
  (async-copy-file-test
   async-copy-file-test-sample-zip-url
   async-copy-file-test-output-dir
   :extract-arg 'unzip
   :finish-fn
   (lambda (_)
     (should
      (and (file-exists-p (concat async-copy-file-test-output-dir "/async-copy-file.el-0.1.0/README.md"))
           (file-exists-p (concat async-copy-file-test-output-dir "/async-copy-file.el-0.1.0/LICENSE")))))))

(ert-deftest async-copy-file-test-url-zip-1 ()
  (async-copy-file-test
   async-copy-file-test-sample-zip-url
   async-copy-file-test-output-dir
   :extract-arg 'unzip
   :finish-fn
   (lambda (_)
     (should
      (and (file-exists-p (concat async-copy-file-test-output-dir "/README.md"))
           (file-exists-p (concat async-copy-file-test-output-dir "/LICENSE")))))))

(ert-deftest async-copy-file-test-url-tar-0 ()
  (async-copy-file-test
   async-copy-file-test-sample-tar-url
   async-copy-file-test-output-dir
   :extract-arg 'tar
   :finish-fn
   (lambda (_)
     (should
      (and (file-exists-p (concat async-copy-file-test-output-dir "/async-copy-file.el-0.1.0/README.md"))
           (file-exists-p (concat async-copy-file-test-output-dir "/async-copy-file.el-0.1.0/LICENSE")))))))

(ert-deftest async-copy-file-test-url-tar-1 ()
  (async-copy-file-test
   async-copy-file-test-sample-tar-url
   async-copy-file-test-output-dir
   :extract-arg 'tar
   :finish-fn
   (lambda (_)
     (should
      (and (file-exists-p (concat async-copy-file-test-output-dir "/README.md"))
           (file-exists-p (concat async-copy-file-test-output-dir "/LICENSE")))))))

(provide 'async-copy-file-test)

;;; async-copy-file-test.el ends here
