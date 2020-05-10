;;; unifdef-test-simple.el --- Simple tests for unifdef

;; Copyright (C) 2020  Anders Lindgren

;; Author: Anders Lindgren

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

;; Tests for the package `unifdef'.

;;; Code:

(require 'unifdef)


;; ------------------------------------------------------------
;; Unit tests.
;;

(defun unifdef-test-guard (str &optional symbol)
  (unless symbol
    (setq symbol "TEST"))
  (with-temp-buffer
    (insert str)
    (goto-char (point-min))
    (unifdef-guard-p symbol)))

(ert-deftest unifdef-guard ()
  (should (equal (unifdef-test-guard "#if TEST")            '(nil . "if")))
  (should (equal (unifdef-test-guard "#if !TEST")           '(t   . "if")))
  (should (equal (unifdef-test-guard "#  if  !  TEST  ")    '(t   . "if")))
  (should (equal (unifdef-test-guard "#  ifdef  TEST  ")    '(nil . "ifdef")))
  (should (equal (unifdef-test-guard "#  ifndef TEST  ")    '(t   . "ifndef")))
  (should (equal (unifdef-test-guard "# if defined  TEST ") '(nil . "if")))
  (should (equal (unifdef-test-guard "# if !defined TEST ") '(t   . "if")))
  (should (equal (unifdef-test-guard "#if defined(TEST)")   '(nil . "if")))
  (should (equal (unifdef-test-guard "#if !defined(TEST)")  '(t   . "if")))
  (should (equal (unifdef-test-guard
                  "#   if   !   defined   (   TEST   )  ")  '(t   . "if")))

  (should (equal (unifdef-test-guard "#if ALPHA")           nil))
  (should (equal (unifdef-test-guard "#if TEST && ALPHA")   nil))
  nil)

;; ------------------------------------------------------------
;; Small full system tests
;;

(defun unifdef-test-string (str &optional symbol negated)
  (unless symbol
    (setq symbol "TEST"))
  (with-temp-buffer
    (c-mode)
    (insert str)
    (unifdef-current-buffer symbol negated)
    (buffer-string)))


(defun unifdef-test-string-negated (str &optional symbol)
  (unifdef-test-string str symbol t))

(defun unifdef-test-string-both (str &optional symbol)
  (cons (unifdef-test-string str symbol)
        (unifdef-test-string-negated str symbol)))

(ert-deftest unifdef-plain-if ()
  ;; --------------------
  ;; Don't crash on trivial cases.
  (should (equal (unifdef-test-string "") ""))
  (let ((s "// A single line-no newline"))
    (should (equal (unifdef-test-string s) s)))
  (let ((s "// A single line\n"))
    (should (equal (unifdef-test-string s) s)))

  ;; --------------------
  ;; Test a simple "if". Also, ensure that it works when it's on the
  ;; first line in the buffer.
  (should (equal (unifdef-test-string-both "\
#if TEST
#endif")
                 '("" . "")))

  ;; --------------------
  ;; if
  (should (equal (unifdef-test-string-both "\
#if TEST
AAA
#endif
")
                 '("AAA\n" . "")))

  ;; --------------------
  ;; if-else
  (should (equal (unifdef-test-string-both "\
#if TEST
AAA
#else
BBB
#endif
")
                 '("AAA\n" . "BBB\n")))

  ;; --------------------
  ;; if, negated
  (should (equal (unifdef-test-string-both "\
#if !TEST
AAA
#endif
")
                 '("" . "AAA\n")))

  ;; --------------------
  ;; if-else, negated
  (should (equal (unifdef-test-string-both "\
#if !TEST
AAA
#else
BBB
#endif
")
                 '("BBB\n" . "AAA\n")))

  ;; --------------------
  ;; Nested, outer rewritten.
  (should (equal (unifdef-test-string-both "\
#if !TEST
#if SOMETHING_ELSE
AAA
#endif
#else
#if SOMETHING_ELSE
BBB
#endif
#endif
")
                 '("\
#if SOMETHING_ELSE
BBB
#endif
"
                   .
                   "\
#if SOMETHING_ELSE
AAA
#endif
")))

  ;; --------------------
  ;; Nested, inner rewritten.
  (should (equal (unifdef-test-string-both "\
#if SOMETHING_ELSE
#if !TEST
AAA
#else
BBB
#endif
#else
#if TEST
CCC
#else
DDD
#endif
#endif
")
                 '("\
#if SOMETHING_ELSE
BBB
#else
CCC
#endif
"
                   .
                   "\
#if SOMETHING_ELSE
AAA
#else
DDD
#endif
")))


  ;; --------------------
  ;; Rewrite elif into if.
  (should (equal (unifdef-test-string-both "\
#if !TEST
AAA
#elif SOMETHING_ELSE
BBB
#endif
")
                 '("\
#if SOMETHING_ELSE
BBB
#endif
"
                   . "AAA\n")))

  ;; --------------------
  ;; Ditto, with whitespace.
  (should (equal (unifdef-test-string-both "\
# if !TEST
AAA
# elif SOMETHING_ELSE
BBB
# endif
")
                 '("\
# if SOMETHING_ELSE
BBB
# endif
"
                  . "AAA\n")))

  nil)


(ert-deftest unifdef-plain-elif ()
  ;; --------------------
  ;; Blocks that doesn't contain the symbol should not be modified.
  (let ((s "\
#if ALPHA
AAA
#elif BETA
BBB
#elif GAMMA
CCC
#endif
"))
    (should (equal (unifdef-test-string-both s) (cons s s))))

  ;; --------------------
  ;; Last #elif before #endif, no content.
  (should (equal (unifdef-test-string-both "\
#if ALPHA
#elif TEST
#endif
")
                 '("\
#if ALPHA
#else
#endif
"
                   . "\
#if ALPHA
#endif
")))


  ;; --------------------
  ;; Ditto, with extra witespace.
  (should (equal (unifdef-test-string-both "\
#if ALPHA
#    elif TEST
#endif
")
                 '("\
#if ALPHA
#    else
#endif
"
                   . "\
#if ALPHA
#endif
")))

  ;; --------------------
  ;; Last #elif before #endif, with content.
  (should (equal (unifdef-test-string-both "\
#if ALPHA
#elif TEST
AAA
#endif
")
                 '("\
#if ALPHA
#else
AAA
#endif
"
                   . "\
#if ALPHA
#endif
")))

  ;; --------------------
  ;; More than one directive with same test.
  (should (equal (unifdef-test-string-both "\
#if !TEST
AAA
#elif !TEST
#elif !TEST
#elif !TEST
#else
BBB
#endif
")
                 '("BBB\n" . "AAA\n")))

  (should (equal (unifdef-test-string-both "\
#if ALPHA
#elif !TEST
AAA
#elif !TEST
#elif !TEST
#else
BBB
#endif
")
                 '("\
#if ALPHA
#else
BBB
#endif
"
                   . "\
#if ALPHA
#else
AAA
#endif
")))


  ;; --------------------
  ;; Mixed.
  (should (equal (unifdef-test-string-both "\
#if !TEST
AAA
#elif !TEST
#elif TEST
BBB
#elif !TEST
#else
#endif
")
                 '("BBB\n" . "AAA\n")))
  (should (equal (unifdef-test-string-both "\
#if ALPHA
#elif !TEST
AAA
#elif !TEST
#elif TEST
BBB
#elif !TEST
#else
#endif
")
                 '("\
#if ALPHA
#else
BBB
#endif
"
                   . "\
#if ALPHA
#else
AAA
#endif
")))

  ;; --------------------
  ;; In string and comment.
  (let ((s "\
/*
#if TEST
#endif
*/

\"
#if TEST
#endif
\"
"))
    (should (equal (unifdef-test-string-both s) (cons s s))))

  nil)

(provide 'unifdef-test-simple)

;;; unifdef-test-simple.el ends here
