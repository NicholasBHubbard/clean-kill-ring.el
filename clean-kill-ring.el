;;; clean-kill-ring.el --- Keep the kill ring clean  -*- lexical-binding: t; -*-

;; Copyright (C) 2025 Nicholas Hubbard <nicholashubbard@posteo.net>
;;
;; Licensed under the same terms as Emacs and under the MIT license.

;; SPDX-License-Identifier: MIT

;; Author: Nicholas Hubbard <nicholashubbard@posteo.net>
;; URL: http://github.com/NicholasBHubbard/clean-kill-ring.el
;; Package-Requires: ((emacs "24.4"))
;; Version: 2.6
;; Created: 2022-02-23
;; By: Nicholas Hubbard <nicholashubbard@posteo.net>
;; Keywords: kill-ring, convenience

;;; Commentary:

;; This package provides functionality for automatic customized filtering of the
;; kill ring.

;;; Code:

(require 'subr-x)

(defcustom clean-kill-ring-filters '(string-blank-p)
  "List of filter functions that if matched keep input out of the `kill-ring'."
  :type '(repeat function)
  :group 'clean-kill-ring-mode)

(defvar clean-kill-ring--start-new-kill-p nil
  "Non-nil when the next appended kill must start a new ring entry.

Killing commands use `kill-append' when the previous command was also a kill.
If that previous kill was filtered, appending normally would modify an unrelated
older entry in the kill ring.")

(defun clean-kill-ring--filter-catch-p (string)
  "T if STRING satisfies at least one of `clean-kill-ring-filters'."
  (let ((caught nil)
        (s (substring-no-properties string)))
    (catch 'loop
      (dolist (filter clean-kill-ring-filters)
        (when (funcall filter s)
          (setq caught t)
          (throw 'loop t))))
    caught))

(defun clean-kill-ring--kill-new-advice (orig-fn &rest args)
  "Advice to `kill-new' when `clean-kill-ring-mode' is enabled.

Prevents input that matches any of the `clean-kill-ring-filters' from entering
the `kill-ring'."
  (let ((input (substring-no-properties (car args))))
    (if (clean-kill-ring--filter-catch-p input)
        (progn
          (setq clean-kill-ring--start-new-kill-p t)
          nil)
      (prog1 (apply orig-fn args)
        (setq clean-kill-ring--start-new-kill-p nil)))))

(defun clean-kill-ring--kill-append-advice (orig-fn &rest args)
  "Advice to `kill-append' when `clean-kill-ring-mode' is enabled.

When the previous logical kill was filtered, start a new kill with the string in
ARGS rather than appending it to an unrelated kill-ring entry.  Otherwise call
ORIG-FN with ARGS normally."
  (if clean-kill-ring--start-new-kill-p
      (kill-new (car args))
    (apply orig-fn args)))

(defvar clean-kill-ring-mode-map (make-sparse-keymap)
  "Keymap for `clean-kill-ring-mode'.")

(define-minor-mode clean-kill-ring-mode
  "Toggle `clean-kill-ring-mode'.

When active prevent strings that satisfy at least one predicate in
`clean-kill-ring-filters' from entering the `kill-ring'."
  :global t
  :group 'clean-kill-ring-mode
  :require 'clean-kill-ring
  (if clean-kill-ring-mode
      (progn
        (setq clean-kill-ring--start-new-kill-p nil)
        (advice-add 'kill-new :around #'clean-kill-ring--kill-new-advice)
        (advice-add 'kill-append :around #'clean-kill-ring--kill-append-advice))
    (advice-remove 'kill-new #'clean-kill-ring--kill-new-advice)
    (advice-remove 'kill-append #'clean-kill-ring--kill-append-advice)
    (setq clean-kill-ring--start-new-kill-p nil)))

(defun clean-kill-ring-unload-function ()
  "Disable `clean-kill-ring-mode' before unloading its library."
  (clean-kill-ring-mode -1)
  nil)

(provide 'clean-kill-ring)

;; Local Variables:
;; indent-tabs-mode: nil
;; End:
;;; clean-kill-ring.el ends here
