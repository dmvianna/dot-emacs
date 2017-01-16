
;;; Javascript
;;; Code:

;;; js2 mode:
(add-to-list 'auto-mode-alist '("\\.js$" . js2-mode))
(add-to-list 'auto-mode-alist '("\\.es6$" . js2-mode))

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
(require 'flycheck)
(setq-default flycheck-disabled-checkers
              (append flycheck-disabled-checkers
                      '(javascript-jshint)))

;; use eslint with web-mode for jsx files
(flycheck-add-mode 'javascript-eslint 'web-mode)

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

;; disable json-jsonlist checking for json files
(setq-default flycheck-disabled-checkers
              (append flycheck-disabled-checkers
                          '(json-jsonlist)))

;;; use local eslint from node_modules before global
;;; http://emacs.stackexchange.com/questions/21205/flycheck-with-file-relative-eslint-executable
(defun my/use-eslint-from-node-modules ()
  (let* ((root (locate-dominating-file
                (or (buffer-file-name) default-directory)
                "node_modules"))
         (eslint (and root
                      (expand-file-name "node_modules/eslint/bin/eslint.js"
                                        root))))
    (when (and eslint (file-executable-p eslint))
      (setq-local flycheck-javascript-eslint-executable eslint))))
(add-hook 'flycheck-mode-hook #'my/use-eslint-from-node-modules)

;; Stylus and jade modes
;;(add-to-list 'load-path "~/.emacs.d/vendor/jade-mode")
    (require 'sws-mode)
    (require 'jade-mode)
    (add-to-list 'auto-mode-alist '("\\.styl$" . sws-mode))

(provide 'javascript-config)

;;; javascript-config.el ends here
