;;; unifdef.el --- Delete code guarded by processor directives

;; Copyright (C) 2020  Anders Lindgren

;; Author: Anders Lindgren
;; Keywords: convenience, languages
;; Version: 0.0.1
;; Created: 2020-04-25
;; URL: https://github.com/Lindydancer/unifdef

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

;; `unifdef' is a package that can delete code guarded by C-style
;; preprocessor directives.

;; Examples:
;;
;; Take this relatively simple code:
;;
;;     #ifdef USELONGLONG
;;     long long x;
;;     #else
;;     long x;
;;     #endif
;;
;; `M-x unifdef-current-buffer RET USELONGLONG RET' will transform
;; this into:
;;
;;     long long x;
;;
;; A (somewhat) more complex piece of code:
;;
;;     #if ALPHA
;;       Alpha();
;;     #elif BETA
;;       Beta();
;;     #elif GAMMA
;;       Gamma();
;;     #else
;;       Delta();
;;     #endif
;;
;; `M-x unifdef-current-buffer RET BETA RET' will transform
;; this into:
;;
;;     #if ALPHA
;;       Alpha();
;;     #else
;;       Beta();
;;     #endif


;; What is converted:
;;
;; This package can convert code that test if a symbol is defined or
;; if a symbol is true.  For example:
;;
;;     #ifdef SYMBOL
;;
;;     #if defined SYMBOL
;;
;;     #if defined(SYMBOL)
;;
;;     #if SYMBOL
;;
;; Or if the symbol is undefined or false:
;;
;;     #ifndef SYMBOL
;;
;;     #if !defined SYMBOL
;;
;;     #if !defined(SYMBOL)
;;
;;     #if !SYMBOL
;;
;; And likewise for `elif'.

;; What is not converted:
;;
;; This package does *not* handle complex expressions involving
;; symbols.  For example:
;;
;;     #if ALPHA && BETA
;;
;;     #if ALPHA == 1234
;;
;;     #if (ALPHA ^ BETA) == 0x1234
;;
;;     #if defined(ALPHA) && defined(BETA)

;; Commands:
;;
;; * `unifdef-current-buffer' -- Convert all occurrences in the
;;   current buffer.
;;
;; * `unifdef-convert-block' -- Convert the next preprocessor block.
;;   (Note, in some cases, this command might need to be applied
;;   repeatedly.)
;;
;; When prefixed with C-u the command is inverted, i.e. it assumes
;; that the supplied symbol is undefined or false.  In the example
;; above, the line `long x;' would be retained.

;; Warning:
;;
;; The command provided by this package can perform massive changes to
;; your source files.  Make sure that they are backed up, e.g. by
;; using a version control system.  As for all tools that
;; automatically transform source code, it is advisable to manually
;; inspect the end result.

;;; Code:

;; A word on naming:
;;
;; * `skip' is used when the current line is included in the
;;   search. (See this as in `skip-chars-forward'.
;;
;; * `forward' is used when the current line is excluded.


(defun unifdef-re-search-forward-in-code (regexp &optional bound noerror count)
  "Like `re-search-forward' but don't match in comments or strings.

Pass REGEXP, BOUND, NOERROR, and COUNT to `re-search-forward'.

Return non-nil if a match is found.

Note: Point may move even if result is nil."
  (let (res)
    (while (and (setq res (re-search-forward regexp bound noerror count))
                (nth 8 (syntax-ppss))))
    res))


(defun unifdef-skip-to-conditional-start ()
  "Skip to the start of the next conditional preprocessor block.

A conditional preprocessor block start with #if, #ifdef or #ifndef.

Return the directive or nil."
  (and (unifdef-re-search-forward-in-code
        "^#[ \t]*\\(if\\(n?def\\)?\\)\\>" nil t)
       (progn
         (beginning-of-line)
         (match-string-no-properties 1))))


(defun unifdef-skip-to-conditional-directive ()
  "Skip to the next conditional preprocessor directive.

Conditional preprocessor directives are #if, #ifdef, #ifndef,
#else, #elif, and #endif.

Return the directive or nil."
  (and (unifdef-re-search-forward-in-code
        "^#[ \t]*\\(if\\|ifn?def\\|else\\|elif\\|endif\\)\\>" nil t)
       (progn
         (beginning-of-line)
         (match-string-no-properties 1))))


(defun unifdef-forward-line ()
  "Move to the next logical line.

A physical line is spliced with the next if it ends with a
backslash character (\\), forming a logical line."
  (while (and (not (eobp))
              (eq (char-before (line-end-position)) ?\\))
    (forward-line))
  (forward-line))


(defun unifdef-forward-to-conditional-same-depth ()
  "Go to the next directive on the same depth as the current.

Return the directive (a string) or nil."
  ;; Skip the directive on the current line.
  (let ((depth 0)
        res)
    (while (progn
             (unifdef-forward-line)
             (and (setq res (unifdef-skip-to-conditional-directive))
                  (cond ((member res '("if" "ifdef" "ifndef"))
                         (setq depth (+ depth 1))
                         t)
                        ((equal res "endif")
                         (setq depth (- depth 1))
                         (not (eq depth -1)))
                        (t
                         (not (eq depth 0)))))))
    (beginning-of-line)
    res))


;; TODO: Handle the case when there is no #endif, e.g. issue an error.
(defun unifdef-skip-to-endif-same-depth ()
  "Move point to the #endif of the current conditional block."
  (unless (looking-at "#endif")
    (while (let ((dir (unifdef-forward-to-conditional-same-depth)))
             (and dir
                  (not (equal dir "endif")))))))


(defun unifdef-guard-p (symbol)
  "Non-nil directive at point check SYMBOL.

Return (NEGATED . DIRECTIVE) where NEGATED is non-nil is t when
the preprocessor directive succeeds when SYMBOL is false or
undefined."
  (let ((ws "[ \t]*")
        (re-symbol "\\([a-zA-Z_][a-zA-Z0-9_]*\\)"))
    (if (and (or (looking-at (concat "#" ws "\\(if\\(n\\)?def\\)\\>" ws
                                     re-symbol ws "$"))
                 (looking-at (concat "#" ws "\\(if\\|elif\\)\\>" ws
                                     "\\(!" ws "\\)?"
                                     re-symbol ws "$"))
                 (looking-at (concat "#" ws "\\(if\\|elif\\)\\>" ws
                                     "\\(!" ws "\\)?"
                                     "defined" ws
                                     "(?" ws re-symbol ws
                                     "\\()" ws "\\)?"
                                     "$")))
             (equal symbol (match-string-no-properties 3)))
        (cons (if (match-beginning 2) t nil)
              (match-string-no-properties 1))
      nil)))


(defun unifdef-convert-block (symbol &optional negated)
  "Remove next preprocessor statements guarded with SYMBOL in buffer.

Do nothing when next preprocessor statement is not guarded by
SYMBOL.

When NEGATED is nil, keep the part where the condition is true,
when non-nil keep the part where it is false.

Point is placed so that subsequent calls will be applied to this
block (when the initial directive was rewritten) or directly
after it.  Effectively, repeated calls will eventually rewrite
all blocks in the buffer.

Return non-nil when a conditional preprocessor block was found."
  (interactive "sSymbol: \nP")
  (when (unifdef-skip-to-conditional-start)
    (let ((pair (unifdef-guard-p symbol)))
      (cond
       (pair
        ;; Rewrite the first directive.
        (if (eq negated (car pair))
            ;; --------------------
            ;; Condition true, keep the first section, delete
            ;; everything else.
            ;;
            ;; #if COND        <- Remove
            ;; ...
            ;; #else  /  #elif <- Remove
            ;; ...             <- Remove
            ;; #endif          <- Remove
            ;;
            ;; Leave point at the beginning of the retained section.
            ;; --------------------
            (progn
              (save-excursion
                (unifdef-forward-to-conditional-same-depth)
                (let ((p (point)))
                  (unifdef-skip-to-endif-same-depth)
                  (unifdef-forward-line)
                  (delete-region p (point))))
              (delete-region (point)
                             (save-excursion
                               (unifdef-forward-line)
                               (point))))
          ;; Condition false, remove the first section.
          (save-excursion
            (let ((p (point))
                  (directive
                   (unifdef-forward-to-conditional-same-depth)))
              (cond ((equal directive "else")
                     ;; --------------------
                     ;; #if COND        <- Remove
                     ;; ...             <- Remove
                     ;; #else           <- Remove
                     ;; ...
                     ;; #endif          <- Remove
                     ;;
                     ;; Leave point at the beginning of the retained
                     ;; section.
                     ;; --------------------

                     ;; Delete the "#endif"
                     (save-excursion
                       (unifdef-skip-to-endif-same-depth)
                       (delete-region (point)
                                      (save-excursion
                                        (unifdef-forward-line)
                                        (point))))
                     ;; Delete "#if ... #else"
                     (delete-region p
                                    (save-excursion
                                      (unifdef-forward-line)
                                      (point))))
                    ((equal directive "elif")
                     ;; --------------------
                     ;; #if COND        <- Remove
                     ;; ...             <- Remove
                     ;; #elif COND2     <- Convert to "if
                     ;; ...
                     ;; #endif
                     ;;
                     ;; Place point at the new "#if", to ensure that
                     ;; subsequent calls will perform further
                     ;; rewrites, if needed.
                     ;; --------------------

                     ;; Delete the "el" in "elif".
                     ;;
                     ;; Note: This code retains the indentation, under
                     ;; the assumption that if indentation is used, it
                     ;; is to distinguish this block from surrounding
                     ;; blocks.
                     (save-excursion
                       (forward-char)   ; Skip #
                       (skip-chars-forward " \t")
                       (delete-region (point) (+ (point) 2)))
                     (delete-region p (point)))
                    ((equal directive "endif")
                     ;; --------------------
                     ;; #if COND        <- Remove
                     ;; ...             <- Remove
                     ;; #endif          <- Remove
                     ;;
                     ;; Leave point where the block used to start.
                     ;; --------------------
                     (delete-region
                      p
                      (save-excursion
                        (unifdef-skip-to-endif-same-depth)
                        (unifdef-forward-line)
                        (point))))
                    (t
                     (error "Unexpected directive `%s'" directive)))))))
       (t
        ;; Iterate over all #elif directives.
        (save-excursion
          (let ((dir (unifdef-forward-to-conditional-same-depth)))
            (while
                (cond
                 ((equal dir "elif")
                  (let ((pair (unifdef-guard-p symbol))
                        (end
                         (save-excursion
                           (setq dir
                                 (unifdef-forward-to-conditional-same-depth))
                           (point))))
                    (if pair
                        ;; This #elif should be modified.
                        (if (eq negated (car pair))
                            ;; --------------------
                            ;; Condition true, keep first section,
                            ;; remove the rest.
                            ;;
                            ;; #elif COND   <- Convert to #else
                            ;; ...
                            ;; #???         <- Remove
                            ;; ...          <- Remove
                            ;; #???         <- Remove
                            ;; ...          <- Remove
                            ;; #endif
                            ;; --------------------
                            (progn
                              ;; Remove remaining sections.
                              (delete-region
                               end
                               (save-excursion
                                 (goto-char end)
                                 (unifdef-skip-to-endif-same-depth)
                                 (point)))
                              ;; Rewrite "elif ..." to "else".
                              (forward-char) ; skip #
                              (skip-chars-forward " \t")
                              (delete-region (point) (line-end-position))
                              (insert "else")
                              nil)
                          ;; --------------------
                          ;; Condition false, remove first section,
                          ;; continue with the next.
                          ;;
                          ;; #elif COND   <- Remove
                          ;; ...          <- Remove
                          ;; #???
                          ;; ...
                          ;; #endif
                          ;; --------------------
                          (delete-region (point) end)
                          t)
                      ;; This #elif shold not be modified.
                      (goto-char end)
                      t)))
                 ((equal dir "else")
                  nil)
                 ((equal dir "endif")
                  nil)
                 (t
                  (error "Unexpected directive %s" dir)
                  nil)))))
        ;; Goto first section in the block.
        (unifdef-forward-line))))
    ;; Indicate that we found a preprocessor clause (even though it's
    ;; not certain we did anything with it).
    t))


(defun unifdef-current-buffer (symbol &optional negated)
  "Remove preprocessor statements guarded with SYMBOL in current buffer.

When NEGATED is nil, keep the part where the symbol is true, when
non-nil keep the part where it is false."
  (interactive "sSymbol: \nP")
  (save-excursion
    (goto-char (point-min))
    (while (unifdef-convert-block symbol negated))))


(provide 'unifdef)

;;; unifdef.el ends here
