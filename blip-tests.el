;;; blip-tests.el --- self-tests tests for blip.el   -*- lexical-binding: t; -*-

;; Copyright (C) 2013  João Távora

;; Author: João Távora <joaotavora@gmail.com>
;; Keywords:

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

;;; Commentary:

;;

;;; Code:

(require 'ert)
(require 'ert-x)
(require 'bytecomp)

(defvar blip--self-tests-file)
(setq blip--self-tests-file (or byte-compile-current-file
                                load-file-name
                                blip--self-tests-file))

(ert-deftest find-test-file ()
  (should (equal (blip--find-test-file "blip.el")
                 blip--self-tests-file) )
  (should (equal (blip--find-test-file
                  (expand-file-name "blip.el"
                                    (file-name-directory
                                     (expand-file-name blip--self-tests-file))))
                 blip--self-tests-file)))

(ert-deftest find-tests-function ()
  (should (equal (blip--run-tests-function blip--self-tests-file)
                 'blip--run-ert-tests)))

;; (ert-deftest slow ()
;;   (sit-for 1)
;;   (ert-pass))


(provide 'blip-tests)
;;; blip-tests.el ends here
