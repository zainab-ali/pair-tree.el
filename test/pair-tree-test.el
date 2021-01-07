;;; pair-tree-test.el --- pair tree tests            -*- lexical-binding: t; -*-

;; Copyright (C) 2021  Zainab Ali

;; Author: Zainab Ali <zainab@kebab-ca.se>

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

;;; Code:

(require 'pair-tree)

;; Point conversion

(ert-deftest pair-tree--grid-pos ()
  "Test that the grid pos can be converted to a buffer pos."
  ;; Consider a 2 column by 3 line grid
  (let* ((poss '( ((0 . 0) . 1)
                  ((1 . 0) . 2)
                  ((0 . 1) . 4)
                  ((1 . 1) . 5)
                  ((0 . 2) . 7)
                  ((1 . 2) . 8))))
    (--each
        poss
      (progn
        (should (equal (pair-tree--grid-to-buffer-pos
                        (pair-tree--pos-make :col (caar it)
                                    :line (cdar it))
                        1)
                       (cdr it)))
        (should (equal (pair-tree--buffer-to-grid-pos (cdr it) 1)
                       (pair-tree--pos-make :col (caar it)
                                   :line (cdar it))))))))


;; Path

(ert-deftest pair-tree--condensed-accessors ()
  "The list must contain all car/cdr forms in the Common Lisp spec.
These should be sorted in descending order of length."
  (should (equal (length pair-tree--condensed-accessors) 30))
  (should (-same-items-p (-take-last 2 pair-tree--condensed-accessors) '("a" "d")))
  (should (equal (length (car pair-tree--condensed-accessors)) 4))
  (should (equal (-distinct (-map #'length pair-tree--condensed-accessors)) '(4 3 2 1))))

(ert-deftest pair-tree--path-condensed ()
  "The condensed path must be the shortest combination of accessors."
  (should (equal (pair-tree--path-condensed '("a" "d")) '("ad")))
  (should (equal (pair-tree--path-condensed '("a" "d" "a" "a" "a")) '("a" "daaa")))
  (should (equal (pair-tree--path-condensed '("a" "d" "d" "d" "a" "d" "a" "a" "a")) '("a" "ddda" "daaa"))))

(ert-deftest pair-tree--path-str ()
  "The expression should evaluate to the element at the path."
  (let ((test (lambda (path el)
                (should (equal
                         (eval (read (format "(let ((tree '(1 2 3))) %s)"
                                             (pair-tree--path-str path))))
                         el)))))
    (funcall test '("a") 1)
    (funcall test '("ad") 2)
    (funcall test '("add") 3)))

(ert-deftest pair-tree--path-print ()
  "The path of a node in a tree should access the element."
  (let ((get-message (lambda ()
                       (with-current-buffer "*Messages*"
               (goto-char (point-max))
               (string-trim (thing-at-point 'line)))))
        (tree (pair-tree--make '((1 . 2) 3)))
        (messages '(((0 . 1) . "(car tree)")
                    ((0 . 2) . "(caar tree)")
                    ((1 . 1) . "(cdar tree)")
                    ((2 . 1) . "(cadr tree)")
                    ((3 . 0) . "(cddr tree)"))))
    (--each messages
      (progn
        (pair-tree--path-print tree (pair-tree--pos-make :col (caar it) :line (cdar it)))
        (should (equal (funcall get-message) (cdr it)))))))

;; Sexp

(ert-deftest pair-tree--write-sexp ()
  "Tests that the correct sexp is written to the *sexp* buffer."
  (--each
      '(nil
        (1 . (2 3))
        ((1 . 2) 3 4)
        (1 (2 . 3) 4))
      (with-temp-buffer
        (pair-tree--write-sexp (pair-tree--make it))
        (should (equal (eval (read (buffer-string))) it)))))

(provide 'pair-tree-test)
;;; pair-tree-test.el ends here
