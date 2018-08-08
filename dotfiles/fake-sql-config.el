;;; fake-sql-config.el
;;; Commentary:
;;; Code:

;;; don't backspace the prompt
(setq comint-prompt-regexp "^SQL>")

(custom-set-variables
'(comint-prompt-read-only t)
'(comint-use-prompt-regexp t))
(custom-set-faces)

;; no carriage return for fetched results
(add-hook 'sql-interactive-mode-hook
          (lambda ()
            (toggle-truncate-lines t)))


;;; connection works if we create the following string
;;; in the shell or via :user and :database within emacs
;;; myuser@mydb:myport/myservicename
;;;
;;; emacs doesn't support Oracle's special servicename parameter
;;; so the config below does not work unless the database is
;;; configured via ~/.tnsnames.ora
;;;
;;; mydb =
;;;   (description =
;;;     (address_list =
;;;       (address= (protocol = TCP)(host = mydb)(port = myport))
;;;     )
;;;     (connect_data =
;;;       (service_name = myservicename)
;;;     )
;;;   )
;;;
;;; If you have one server that you usually connect to, you can set
;;; its login parameters as default using the variables
;;; sql-***-login-params where *** can be one of the database type that you want, for example

(setq sql-oracle-login-params
      '((user :default "myuser")
        (database :default "mydb")
        ))

(provide 'sql-config)

;;; sql-config.el ends here

