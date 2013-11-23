;;; blip.el --- automatically find and run tests in emacs   -*- lexical-binding: t; -*-

;; Copyright (C) 2013  João Távora

;; Author: João Távora <joaotavora@gmail.com>
;; Keywords: convenience

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
;;;
;;;  [This does more or less the same as
;;;   http://www.emacswiki.org/emacs/unit-test.el but I didn't know of
;;;   it, and also NIH and also racecar. Still, pixmap-generating code
;;;   stolen from it, thanks very much!]
;;;
;;; Use `M-x blip-mode' to enable `blip-mode', which runs tests whenever
;;; you save the buffer.
;;;
;;; Tests are deduced using the rules in the variable
;;; `blip-find-test-file-rules'
;;;
;;; The function to perform the testing is deduced using the variable
;;; `blip-run-functions'. Currently only emacs24's own `ert'-based tests
;;; are supported through `blip--run-ert-tests', which is bundled.
;;;
;;; Both `blip-find-test-file-rules' and `blip-run-functions' are intended
;;; to be customized by the user, see their doc.
;;;
;;; TODO: more self tests
;;; TODO: more functions to run tests in other languages/frameworks
;;; TODO: fix bugs and cleanup
;;;
;;; Code:

(defvar blip-find-test-file-rules
  '((basename "-test" "." extension)
    (basename "-tests" "." extension)
    ("/tests/" basename "." extension)
    ("/test/" basename "." extension)
    ;; the test file might be the file itself
    (basename "." extension))
  "Rules to derive test file names from source file names.

Value is a list of symbols and strings concatenated to produce a
relative of absolute pathname of the test file. The following
symbols can be used:

* `basename' - evaluates to the basename (no directory, no
  extension) of the source file
* `extension' - evaluates to the directory of the source file
* `project-dir' - evaluates to the project directory where the source
  is located.

For the source file \"foo.el\", an entry:

  (basename \"-test\" \".\" extension)

will produce the relative pathname \"foo-test.el\", which will
match that file if it leaves beside \"foo.el\".

An entry

  (project-dir \"/tests/\" basename \".\" extension)p

will work if your tests are located in a separate directory ")

(defvar blip-run-functions
  `((".*\.el$" . blip--run-ert-tests)
    (".*\.lisp$" . blip--run-stefil-tests-with-slime))
"Decides which function should run tests for each file.

Value of this variable is an alist of (REGEXP . FUNCTION).  When
a REGEXP matches the test file name, the FUNCTION is selected for
actually performing the test.

FUNCTION is passed 4 arguments:

1. The test file;
2. The source file or nil if the source buffer hasn't been saved yet;
3. A boolean suggesting that the test file be reloaded;
4. A boolean suggesting that the source file be reloaded;

Reloading suggestions are calculated from file mtimes and from
buffer-modification status in the files are opened in
emacs. FUNCTION is free to ignore the suggestions.

FUNCTION is responsible for running the tests and returning three values

1. A boolean indicating if the tests passed.
2. A buffer name or buffer object containing a description of the tests
3. A short descriptive message summarizing the tests run")



;;; Helper fns
;;;
(defun blip--test-file (src-file rule)
  (loop for token in rule
        concat (pcase token
                 (`basename (file-name-base src-file))
                 (`extension (file-name-extension src-file))
                 (`project-dir (locate-dominating-file src-file ".git"))
                 (t token))))

(defun blip--find-test-file (src-file)
  (loop for rule in blip-find-test-file-rules
        for test-file = (expand-file-name
                         (blip--test-file src-file rule)
                         (file-name-directory src-file))
        when (file-readable-p test-file)
        return (expand-file-name test-file )))

(defvar blip--run-tests-function nil)
(put 'blip--run-tests-function 'risky-local-variable t)
(put 'blip-mode 'risky-local-variable nil)



(defun blip--guess-run-tests-function (test-file)
  (loop for (regexp . function) in blip-run-functions
        when (string-match regexp test-file)
        return function))

(defun blip--run-tests-function (test-file)
  (or blip--run-tests-function
      (blip--guess-run-tests-function test-file)))

(defvar blip--file-mtimes (make-hash-table))

(defun blip--file-might-need-reloading-p (file)
  (let ((buffer (find-buffer-visiting file))
        (mtime (nth 5 (file-attributes file)))
        (last-mtime (gethash file blip--file-mtimes)))
    (or (and buffer
             (buffer-modified-p buffer))
        (and mtime
             (or (not last-mtime)
                 (time-less-p last-mtime mtime))))))



;;; mode line
;;;
(defvar blip--colours '(("orange"      . "#FF9900")
                            ("dark-orange" . "#E86400")
                            ("green"       . "#00FF00")
                            ("dark-green"  . "#00C400")
                            ("red"         . "#FF0000")
                            ("dark-red"    . "#C40000")))

(defun blip--dot (colour)
  "Return an XPM string representing a dot whose colour is COLOUR."
  (format "/* XPM */
static char * test_pass_xpm[] = {
\"18 13 4 1\",
\" 	c None\",
\".	c #000000\",
\"+	c %s\",
\"c	c %s\",
\"                  \",
\"       .....      \",
\"      .ccccc.     \",
\"     .cc+++cc.    \",
\"    .cc+++++cc.   \",
\"    .c+++++++c.   \",
\"    .c+++++++c.   \",
\"    .c+++++++c.   \",
\"    .cc+++++cc.   \",
\"     .cc+++cc.    \",
\"      .ccccc.     \",
\"       .....      \",
\"                  \"};"
          (cdr (assoc colour blip--colours))
          (cdr (assoc (concat "dark-"colour) blip--colours))))

(defvar blip--last-results nil)

(defun blip--find-output-buffer (&optional _event)
  (interactive "e")
  (switch-to-buffer (second blip--last-results)))

(defun blip--mode-line-indicator ()
  (let ((map (make-sparse-keymap)))
    (define-key map [mode-line mouse-1] 'blip--find-output-buffer)
    (if blip--last-results
        `(" blip"
          (:propertize "yo"
                       help-echo ,(third blip--last-results)
                       keymap ,map
                       display
                       (image :type xpm
                              :data ,(cond ((eq (first blip--last-results)
                                                'running)
                                            (blip--dot "orange"))
                                           ((first blip--last-results)
                                            (blip--dot "green"))
                                           (t
                                            (blip--dot "red")))
                              :ascent center)))
      " blip(?)")))

(defun blip--run-tests-using-function (function test-file src-file buffer)
  (cl-flet ((echo-message (passed message output-buffer)
                          (message "Tests for %s in %s %s %s(see buffer %s)"
                                   (or (and src-file
                                            (file-name-nondirectory src-file))
                                       buffer)
                                   (file-name-nondirectory test-file)
                                   (if passed "passed" "failed")
                                   (if message (format " with message \"%s\" " message) "")
                                   output-buffer)))
    (set (make-local-variable 'blip--last-results)
         (list 'running nil "Running tests for %s in %s"
               (or (and src-file
                        (file-name-nondirectory src-file))
                   buffer)
               (file-name-nondirectory test-file)))
    (funcall function test-file
             #'(lambda (message output-buffer)
                 (with-current-buffer buffer
                   (set (make-local-variable 'blip--last-results)
                        (list t output-buffer message))
                   (echo-message t message output-buffer)))
             #'(lambda (message output-buffer)
                 (with-current-buffer buffer
                   (set (make-local-variable 'blip--last-results)
                        (list nil output-buffer message))
                   (echo-message t message output-buffer)))
             :src-file src-file
             :reload-test (blip--file-might-need-reloading-p test-file)
             :reload-source (and src-file (blip--file-might-need-reloading-p src-file)))))


;;; Minor mode and main entry point
;;;
(defun blip-run-tests (&optional buffer)
  (interactive "b")
  (let* ((buffer (or (and buffer (get-buffer buffer))
                     (current-buffer)))
         (src-file (buffer-file-name buffer))
         (test-file (blip--find-test-file (or src-file
                                              (buffer-name buffer))))
         (function (and test-file
                        (blip--run-tests-function test-file))))
    (cond ((and test-file
                (not function))
           (display-warning 'blip (format "don't know how to run the tests for %s in %s"
                                          (file-name-nondirectory src-file)
                                          (file-name-nondirectory test-file))))
          ((and src-file (not test-file))
           (display-warning 'blip (format "can't find the tests for %s"
                                          (file-name-nondirectory src-file))))
          ((not test-file)
           (display-warning 'blip (format "can't find the tests for buffer %s" buffer)))
          (t
           (message (format "running the tests for %s in %s using %s"
                            (file-name-nondirectory src-file)
                            (file-name-nondirectory test-file)
                            (or (and (symbolp function)
                                     function)
                                "a suitable but anonymous function")))
           (blip--run-tests-using-function function test-file src-file buffer)))))

(define-minor-mode blip-mode "A minor mode for running unit-tests automatically"
  nil (:eval
       (blip--mode-line-indicator))
  :group 'blip
  (cond (blip-mode
         (add-hook 'after-save-hook
                   'blip-run-tests 'append 'local)
         (blip-run-tests))
        (t
         (remove-hook 'after-save-hook
                      'blip-run-tests 'local))))



;;; Some run-tests functions
;;;
(require 'ert)
(require 'ert-x)


(cl-defun blip--run-ert-tests (test-file success-fn failed-fn &key
                                         src-file
                                         _reload-source
                                         reload-test)
  (when reload-test
    (ert-delete-all-tests)
    (load-file test-file))
  ;; (when reload-source
  ;;   ( src-file 'load))
  (with-temp-buffer
    ;; HACK: Use letf here to neutralize 'pop-to-buffer, which is
    ;; always used by ert. `display-buffer-*' variables don't do the
    ;; trick because they need to return a window selecting the buffer
    ;;
    (letf (((symbol-function 'pop-to-buffer) #'(lambda (buffer &optional _action _norecord)
                                                 (set-buffer buffer))))
      (let* ((output-buffer (get-buffer-create
                             (format "*blip-ert-tests-for-%s*" (file-name-nondirectory src-file))))
             (stats (ert t output-buffer))
             (total (and stats (ert-stats-total stats)))
             (completed (and stats (ert-stats-completed stats)))
             (unexpected (and stats (ert-stats-completed-unexpected stats)))
             (skipped (and stats (ert-stats-skipped stats))))
        (funcall (if (and total completed unexpected skipped
                          (zerop unexpected))
                     success-fn
                   failed-fn)
                 (format "%d total %d completed %d unexpected %d skipped"
                         total completed unexpected skipped)
                 output-buffer)))))



(provide 'blip)
;;; blip.el ends here
