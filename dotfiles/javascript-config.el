
;; Javascript


;; JavaScript autocompletion and introspection
(add-hook 'js-mode-hook (lambda () (tern-mode t)))
(add-hook 'tern-mode-hook 'auto-complete-mode)
(eval-after-load 'tern
  '(progn
  (require 'tern-auto-complete)
  (tern-ac-setup)))

;; Enable ES6
;; disable jshint since we prefer eslint
(require 'flycheck)
(setq-default flycheck-disabled-checkers
              (append flycheck-disabled-checkers
                      '(javascript-jshint)))

;; use eslint with web-mode for jsx files
(flycheck-add-mode 'javascript-eslint 'web-mode)

;; JSX syntax highlighting

(add-to-list 'auto-mode-alist '("\\.jsx$" . web-mode))
(defadvice web-mode-highlight-part (around tweak-jsx activate)
  (if (equal web-mode-content-type "jsx")
      (let ((web-mode-enable-part-face nil))
        ad-do-it)
    ad-do-it))

;; Stylus and jade modes
;;(add-to-list 'load-path "~/.emacs.d/vendor/jade-mode")
    (require 'sws-mode)
    (require 'jade-mode)
    (add-to-list 'auto-mode-alist '("\\.styl$" . sws-mode))

(provide 'javascript-config.el)

;; javascript-config.el ends here
