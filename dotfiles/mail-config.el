;;; mail-config.el -- Summary:
;;;
;;; Configuration for handling email
;;;
;;; Commentary:
;;; Relies on `getmail` and `mu` being configured at the system level.
;;; Also depends on the MU4E environment variable containing the
;;; file location of `mu4e`, which is part of the `mu` package at
;;; share/emacs/site-lisp/mu4e
;;;
;;; Code:

(setq load-path (append load-path '((getenv "MU4E"))))
(require 'mu4e)

(setq mu4e-get-mail-command "getmail"
      mu4e-update-interval 300
      mu4e-attachment-dir "~/Downloads")

(provide 'mail-config)
