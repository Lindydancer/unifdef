;;; unifdef-test-setup.el --- Execute unifdef tests. -*- lexical-binding: t; -*-

;;; Commentary:

;; This package sets up a suitable enviroment for testing
;; unifdef, and executes the tests.
;;
;; Usage:
;;
;;   emacs -q -l unifdef-test-setup.el
;;
;; Note that this package assumes that some packages are located in
;; specific locations.
;;
;; Note that different Emacs versions highlight Objective-C slightly
;; differently. The corresponding .faceup file was generated using
;; Emacs 24.3.

;;; Code:

(setq inhibit-startup-screen t)

(defvar unifdef-test-setup-directory
  (if load-file-name
      (file-name-directory load-file-name)
    default-directory))

(dolist (dir '("." ".."))
  (add-to-list 'load-path (concat unifdef-test-setup-directory dir)))

(require 'unifdef)
(require 'unifdef-test-simple)

(ert t)

;;; unifdef-test-setup.el ends here
