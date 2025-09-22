;;; clean-kill-ring.el --- Keep the kill ring clean  -*- lexical-binding: t; -*-

;; Copyright (C) 2025 Nicholas Hubbard <nicholashubbard@posteo.net>
;;
;; Licensed under the same terms as Emacs and under the MIT license.

;; SPDX-License-Identifier: MIT

;; Author: Nicholas Hubbard <nicholashubbard@posteo.net>
;; URL: http://github.com/NicholasBHubbard/clean-kill-ring.el
;; Package-Requires: ((emacs "24.4"))
;; Version: 2.0
;; Created: 2022-02-23
;; By: Nicholas Hubbard <nicholashubbard@posteo.net>
;; Keywords: kill-ring, convenience

;;; Commentary:

;; This package provides functionality for automatic customized filtering of the
;; kill ring.

;;; Code:

(defcustom clean-kill-ring-filters '(string-blank-p)
  "List of filter functions that if matched keep input out of the `kill-ring'."
  :type '(repeat function)
  :group 'clean-kill-ring-mode)

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
    (unless (clean-kill-ring--filter-catch-p input)
      (apply orig-fn args))))

(defvar clean-kill-ring-mode-map (make-sparse-keymap)
  "Keymap for `clean-kill-ring-mode'.")

(define-minor-mode clean-kill-ring-mode
  "Toggle `clean-kill-ring-mode'.

When active prevent strings that satisfy at least one predicate in
`clean-kill-ring-filters' from entering the `kill-ring'."
  :global t
  :group 'clean-kill-ring-mode
  :require 'clean-kill-ring
  :keymap clean-kill-ring-mode-map
  (if clean-kill-ring-mode
      (progn
        (advice-add 'kill-new :around #'clean-kill-ring--kill-new-advice))
    (advice-remove 'kill-new #'clean-kill-ring--kill-new-advice)))

(provide 'clean-kill-ring)

;; Local Variables:
;; indent-tabs-mode: nil
;; End:
;;; clean-kill-ring.el ends here
