;;; python-config.el -- Summary:
;;;
;;; This is where I configure most of my
;;; Python development environment
;;;
;;; Commentary:
;;; `flycheck-mypy` provides mypy support (static type validation in Python).
;;; This does not work with flymake (used by elpy).
;;; `elpy` provides a lot of IDE support, including running both `flake8` and
;;; `pylint` via flymake. I wasn't able to run either alongside mypy on
;;; flycheck, but they seem to work well in parallel flycheck and flymake
;;; processes.
;;;
;;; Everything hinges on selecting a `pyvenv-workon` *prior* to opening any
;;; Python file. After that, one has to restart emacs and try again.
;;;
;;; 2021 update: lsp took over syntax checking. Flymake does nothing here,
;;; and I left it as a fallback. Apart from (elpy-enable), most everything
;;; in this file is unnecessary.
;;;
;;; Code:


;; (pyenv-mode)
;; (use-package pyenv-mode-auto)

(defun flycheck-python-setup ()
  (flycheck-mode))
(add-hook 'python-mode-hook #'flycheck-python-setup)

;; disable flymake
(when (load "flycheck" t t)
  (setq elpy-modules nil)
  (setq elpy-modules (delq 'elpy-module-flymake elpy-modules))
  (add-hook 'elpy-mode-hook 'flycheck-mode))


;; create a checker for mypy
(flycheck-define-checker
    python-mypy ""
    :command ("mypy"
              "--ignore-missing-imports" "--fast-parser"
              "--python-version" "3.9"
              source-original)
    :error-patterns
    ((error line-start (file-name) ":" line ": error:" (message) line-end))
    :modes python-mode)

(add-to-list 'flycheck-checkers 'python-mypy t)

(setq elpy-rpc-python-command
      (cond
       ((eq system-type 'darwin) "~/.pyenv/shims/python3")
       ((eq system-type 'gnu/linux) "python3")
       )
      elpy-syntax-check-command "flake8"
      python-shell-interpreter "jupyter"
      python-shell-interpreter-args "console --simple-prompt"
      python-shell-prompt-detect-failure-warning nil
      )

(use-package python-black
 ;; :demand t
 :after python)
(elpy-enable)

(add-to-list 'python-shell-completion-native-disabled-interpreters
             "jupyter")

(add-hook 'python-mode-hook
          (lambda ()
            (setq flycheck-python-pylint-executable "pylint")
            (setq flycheck-pylintrc "/home/dmvianna/.pylintrc")))
(setq flycheck-checker 'python-pylint)
(flycheck-add-next-checker 'python-pylint 'python-mypy)
(flycheck-add-next-checker 'python-mypy 'python-flake8)



(provide 'python-config)
