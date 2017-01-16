
;;; Javascript
;;; Code:

;;; js2 mode:
(add-to-list 'auto-mode-alist '("\\.js$" . js2-mode))
(add-to-list 'auto-mode-alist '("\\.es6$" . js2-mode))
(setq js2-strict-missing-semi-warning nil)
;; (setq js2-mode-show-parse-errors nil)
;; (setq js2-mode-show-strict-warnings nil)

;;; Web mode:
(require 'web-mode)
(add-hook 'web-mode-hook
          (lambda ()
            (setq web-mode-attr-indent-offset 2)
            (setq web-mode-style-padding 2)
            (setq web-mode-markup-indent-offset 2)
            (setq web-mode-code-indent-offset 2)
            (setq web-mode-css-indent-offset 2)
            (setq web-mode-enable-css-colorization t)
            ))

(setq web-mode-ac-sources-alist
      '(("css" . (ac-source-css-property))
        ("html" . (ac-source-words-in-buffer ac-source-abbrev))))

(defun rm-maybe-jsx-mode ()
  (when (string-equal "jsx" web-mode-content-type)
    (js2-minor-mode 1)))
(add-hook 'web-mode-hook 'rm-maybe-jsx-mode)
(add-to-list 'web-mode-content-types '("jsx" . "\\.jsx?\\'"))
(add-to-list 'auto-mode-alist '("\\.css?\\'" . web-mode))
(add-to-list 'auto-mode-alist '("\\.html?\\'" . web-mode))
(add-to-list 'auto-mode-alist '("\\.jsx?\\'" . web-mode))

;; disable jshint since we prefer eslint
(use-package flycheck
  :ensure t
  :diminish flycheck-mode
  :config
  (global-flycheck-mode)
  
  (setq-default flycheck-disabled-checkers
                (append flycheck-disabled-checkers
                        '(javascript-jshint)))

  ;; use eslint with web-mode for jsx files
  (setq flycheck-checkers '(javascript-eslint))
  ;; use eslint with web-mode for jsx files
  (flycheck-add-mode 'javascript-eslint 'web-mode)
  (flycheck-add-mode 'javascript-eslint 'js2-mode)
  (flycheck-add-mode 'javascript-eslint 'js-mode)
  ;; disable json-jsonlist checking for json files
  (setq-default flycheck-disabled-checkers
                (append flycheck-disabled-checkers
                        '(json-jsonlist)))
  )

;; JSX syntax highlighting

(add-to-list 'auto-mode-alist '("\\.jsx?" . web-mode))
(defadvice web-mode-highlight-part (around tweak-jsx activate)
  (if (equal web-mode-content-type "jsx")
      (let ((web-mode-enable-part-face nil))
        ad-do-it)
    ad-do-it))

(defun my-web-mode-hook ()
  "Hooks for Web mode."
    ;;; http://web-mode.org/
  (web-mode-use-tabs)
  (setq web-mode-markup-indent-offset 2)
  (setq web-mode-css-indent-offset 2)
  (setq web-mode-code-indent-offset 2)
  (lambda () (tern-mode t))
  (add-hook 'tern-mode-hook 'auto-complete-mode)
  (eval-after-load 'tern
    '(progn
       (require 'tern-auto-complete)
       (tern-ac-setup)))
  )
(add-hook 'web-mode-hook  'my-web-mode-hook)

(provide 'javascript-config)

;;; javascript-config.el ends here
