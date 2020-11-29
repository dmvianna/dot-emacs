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
(require 'org-mu4e)
(require 'mu4e-contrib)
(require 'smtpmail)

(auth-source-pass-enable)
(setq mu4e-get-mail-command "mbsync -a"
      mu4e-update-interval 300
      mu4e-attachment-dir "~/Downloads"

      message-send-mail-function 'smtpmail-send-it
      smtpmail-stream-type 'starttls
      smtpmail-default-smtp-server "smtp.gmail.com"
      smtpmail-smtp-server "smtp.gmail.com"
      smtpmail-smtp-service 587
      auth-sources '(password-store)
      auth-source-do-cache nil
      auth-source-debug t
      message-kill-buffer-on-exit t

      mu4e-compose-dont-reply-to-self t
      mu4e-sent-messages-behavior 'delete
      mu4e-view-show-images t
      smtpmail-debug-info t

      mu4e-sent-folder (concat (getenv "HOME") "/Maildir/gmail/Sent Messages")
      mu4e-drafts-folder (concat (getenv "HOME") "/Maildir/gmail/Drafts")
      )

(provide 'mail-config)
