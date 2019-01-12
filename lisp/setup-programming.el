;;; Package --- Summary
;;; Commentary:
;;; Code:

;;; Setup javascript

(use-package js2-mode
  :ensure t
  :init
    (add-to-list 'auto-mode-alist '("\\.js\\'" . js2-mode))
    ;; Required to make imenu functions work correctly
    (add-hook 'js2-mode-hook 'js2-imenu-extras-mode)
  :config
  ;;; Unbind since we want to use xref-js2 instead
  (define-key js-mode-map (kbd "M-.") nil)
  (setq js-indent-level 2)
  )

(use-package js2-refactor
  :ensure t
  :init
  (add-hook 'js2-mode-hook 'js2-refactor-mode)
  (js2r-add-keybindings-with-prefix "C-c C-r"))

(use-package xref-js2
  :ensure t
  :init
  (add-hook 'js2-mode-hook (lambda ()
			     (add-hook 'xref-backend-functions #'xref-js2-xref-backend nil t)))
  )

(provide 'setup-programming)
;;; setup-programming.el ends here
