;;; font-lock-studio-test-edebug.el --- Test f-l-s edebug features  -*- lexical-binding: t; -*-

;; Copyright (C) 2025  Anders Lindgren

;; Author: Anders Lindgren

;; This program is free software: you can redistribute it and/or modify
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

;; Tests for interactive Edebug features of the package Font Lock
;; Studio.
;;
;; This test package use `recursive-edit' to execute a number of input
;; events that corresponds to the keys a user would use in the Font
;; Lock Studio interface buffer.
;;
;; Unfortunately, Edebug makes life somewhat hard since it clears
;; `unread-command-events' when it is launched.  To workaround this
;; problem, the keys that should be sent to Edebug are injected using
;; a hook.

;; Older Emacs Versions:
;;
;; Emacs 27:
;;
;; This test fails on Emacs 27.  When stepping into a expression using
;; Edebug, the match data is incorrect.  However, doing the same
;; operation interactively works correctly.  This must be a bug in
;; Edebug in Emacs 27, probably related to it's `input-pending-p'
;; handling.
;;
;; Emacs 24:
;;
;; In Emacs 24, there is no `edebug-mode-hook', hence the Edebug key
;; injection doesn't work.

;;; Code:

(require 'edebug)

;; ------------------------------------------------------------
;; Dummy major mode used in tests
;;

(defun font-lock-studio-test-edebug-dummy-function (limit)
  "Dummy function used to test \"step into and debug\"."
  ;; Need at least one expression for `edebug' to work.
  (+ 1 2)
  nil)


(defvar font-lock-studio-test-edebug-keywords
  `((font-lock-studio-test-edebug-dummy-function
     (0 'font-lock-keyword-face))
    ;; This will remain a lambda in the keywords.
    ((lambda (limit)
       (+ 1 2)
       nil)
     (0 'font-lock-keyword-face))
    ;; This will be converted to a closure.
    (,(lambda (limit)
        (+ 1 2)
        nil)
     (0 'font-lock-keyword-face))
    ;; Rule with face expression.
    ((lambda (limit)
       (re-search-forward "\\(alpha\\|beta\\)" limit t))
     (0 (if (string= (match-string-no-properties 0) "alpha")
            'font-lock-variable-name-face
          'font-lock-builtin-face)))
    ;; Anchored
    ((lambda (limit)
       (re-search-forward "^values:" limit t))
     ((lambda (limit)
        (re-search-forward "[0-9]+" limit t))
      ;; Pre-match-form
      (progn
        (goto-char (match-end 0))
        ;; Evalute to search limit.
        (line-end-position))
      ;; Post-match-form
      (end-of-line)
      ;; Highlights
      (0 (if (string= (match-string-no-properties 0) "100")
             'font-lock-warning-face
           'font-lock-constant-face)))))
  "Font Lock keywords for Dummy mode used for testing Font Lock Studio.

The keywords are designed to test debugging parts of them using
EDebug.")


(define-derived-mode font-lock-studio-test-edebug-mode fundamental-mode
  "Font-Lock-Studio-Test-Edebug"
  "Major mode for testing Edebug features of Font Lock Studio."
  (setq font-lock-defaults '(font-lock-studio-test-edebug-keywords)))


;; ------------------------------------------------------------
;; Log
;;

(defvar font-lock-studio-test-edebug-log-list '()
  "List of things that happens in this test.

This is typically let-bound.")


(defun font-lock-studio-test-edebug-log (what)
  "Add WHAT to the head of `font-lock-studio-test-edebug-log-list'.

This is used by the regression test to check that WHAT has happend."
  (push what font-lock-studio-test-edebug-log-list))


;; ------------------------------------------------------------
;; Support functions
;;

(defun font-lock-studio-test-edebug-make-edebug-go ()
  "Make Edebug start to debug immediately.

Add this to `edebug-mode-hook' to make Edebug start debugging as
soon as it is launched.

This is implemented by adding \"g\" to the current input events,
making Edebug launch the command `edebuug-go-mode'.

Note: Normally, a caller could accomplish this by adding the key
to `unread-command-events' itself.  Unfortunately, in this case
this is not possible since Edebug internally clears
`unread-command-events'."
  (push (car (font-lock-studio-test-edebug-keys-to-events (list "g")))
        unread-command-events))


(defun font-lock-studio-test-edebug-log-edebug-state ()
  "Log that edebug was or stopped."
  (font-lock-studio-test-edebug-log
   (if edebug-mode
       (list 'edebug-started
             (if (buffer-file-name)
                 (file-name-nondirectory
                  (buffer-file-name))
               (buffer-name))
             (with-current-buffer "*Font Lock Studio*"
               (font-lock-studio-explain-state-at-point)))
     'edebug-stopped)))


(defun font-lock-studio-test-edebug-keys-to-events (keys)
  "Convert KEYS to a list suitable for `unread-command-events'.

If KEYS contains the element \" \", it is treated as a space
event.  However, spaces in other contexts, like \"M-x a-function
RET\" are typically ignored."
  (apply #'append
         (mapcar (lambda (key)
                   ;; `kbd' returns "" when applied to " ".
                   (if (equal key " ")
                       '(32)
                     (listify-key-sequence (kbd key))))
                 keys)))


(defun font-lock-studio-test-edebug-check (&rest keys)
  "In a dummy buffer, launch Font Lock Studio and apply KEYS.

Return a log of events."
  (setq font-lock-studio-test-edebug-log-list '())
  (let ((unread-command-events
         (append
          (font-lock-studio-test-edebug-keys-to-events keys)
          ;; Exit the `recursive-edit' call.
          ;;
          ;; Uncomment the following line to stay in recursive edit.
          (listify-key-sequence (kbd "C-M-c"))
          unread-command-events)))
    (with-temp-buffer
      ;; --------------------
      ;; Create a dummy source buffer
      (font-lock-studio-test-edebug-mode)
      (insert "alpha\n")
      (insert "beta\n")
      (insert "values: 5 100 5000\n")
      ;; --------------------
      ;; Launch Font Lock Studio to debug the font-lock keywords
      (font-lock-studio)
      ;; `unread-command-events' is let-bound by Edebug so it can't be
      ;; used to inject Edebug-related keys.  Instead, this is done by
      ;; a hook function.
      (let ((edebug-hook-funcs
             '(font-lock-studio-test-edebug-make-edebug-go
               font-lock-studio-test-edebug-log-edebug-state)))
        (dolist (func edebug-hook-funcs)
          (add-hook 'edebug-mode-hook func))
        (unwind-protect
            ;; This will release the injected keys in the Font Lock
            ;; Studio interface buffer.
            (recursive-edit)
          (dolist (func edebug-hook-funcs)
            (remove-hook 'edebug-mode-hook func))))
      (prog1
          (reverse font-lock-studio-test-edebug-log-list)
        (with-current-buffer (get-buffer "*Font Lock Studio*")
          (font-lock-studio-quit))))))


;; ------------------------------------------------------------
;; Tests
;;

(ert-deftest font-lock-studio-test-edebug ()
  (should (equal (font-lock-studio-test-edebug-check)
                 '()))
  ;; Step into a function in a file.
  (should (equal (font-lock-studio-test-edebug-check
                  "E")
                 '((edebug-started "font-lock-studio-test-edebug.el"
                                   "Keyword with function name matcher")
                   edebug-stopped)))
  ;; Step into a lambda in the Font Lock Studio interface buffer.
  (should (equal (font-lock-studio-test-edebug-check
                  "n" "E")
                 '((edebug-started "*Font Lock Studio*"
                                   "Keyword with code-based matcher")
                   edebug-stopped)))
  ;; Step into a closure (displayed as a lambda) in the Font Lock
  ;; Studio interface buffer.
  (should (equal (font-lock-studio-test-edebug-check
                  "n" "n" "E")
                 '((edebug-started "*Font Lock Studio*"
                                   "Keyword with code-based matcher")
                   edebug-stopped)))
  ;; Step into a face expression.
  (should (equal (font-lock-studio-test-edebug-check
                  "n" "n" "n" " " "E")
                 '((edebug-started "*Font Lock Studio*"
                                   "Highlight: Face is decided by expression")
                   edebug-stopped)))
  ;; Anchored keywords
  (should (equal (font-lock-studio-test-edebug-check
                  "n" "n" "n" "n"
                  " "                   ; Step matcher
                  " "                   ; Step anchored rule
                  "E")                  ; EDebug pre-match form
                 '((edebug-started "*Font Lock Studio*"
                                   "Pre-match form of anchored highlight")
                   edebug-stopped)))

  (should (equal (font-lock-studio-test-edebug-check
                  "n" "n" "n" "n"
                  " "                   ; Step matcher
                  " "                   ; Step anchored rule
                  " "                   ; Step pre-match form
                  "E")                  ; EDebug highlight
                 '((edebug-started
                    "*Font Lock Studio*"
                    "Anchored highlight with code-based matcher")
                   edebug-stopped)))

  (should (equal (font-lock-studio-test-edebug-check
                  "n" "n" "n" "n"
                  " "                   ; Step matcher
                  " "                   ; Step anchored rule
                  " "                   ; Step pre-match form
                  " "                   ; Step inner matcher ("5")
                  "E")                  ; EDebug highlight
                 '((edebug-started
                    "*Font Lock Studio*"
                    "\
Highlight inside anchored highlight, Face is decided by expression")
                   edebug-stopped)))
  (should (equal (font-lock-studio-test-edebug-check
                  "n" "n" "n" "n"
                  " "                   ; Step matcher
                  " "                   ; Step anchored rule
                  " "                   ; Step pre-match form
                  " "                   ; Step inner matcher ("5")
                  " "                   ; Step highlight
                  " "                   ; Step inner matcher ("100")
                  " "                   ; Step highlight
                  " "                   ; Step inner matcher ("5000")
                  " "                   ; Step highlight
                  " "                   ; Step inner matcher (fail)
                  "E")                  ; EDebug post-match form
                 '((edebug-started
                    "*Font Lock Studio*"
                    "Post-match form of anchored highlight")
                   edebug-stopped))))

(provide 'font-lock-studio-test-edebug)

;;; font-lock-studio-test-edebug.el ends here
