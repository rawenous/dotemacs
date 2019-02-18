;;; Package --- Summary
;;; Commentary:
;;; Code:

;;; Setup javascript


(use-package js2-mode
  :ensure t
  :init
    (add-to-list 'auto-mode-alist '("\\.js\\'" . js2-mode))
    (add-hook 'js2-mode-hook #'lsp)
  :config
  (setq js-indent-level 2)
  (eval-after-load 'lsp-mode
    (general-define-key
     :keymaps 'js2-mode-map
     :prefix "C-c c"
     "r" 'lsp-rename
     "p" 'lsp-ui-peek-find-implementation
     "h" 'lsp-describe-thing-at-point
     "H" 'lsp-describe-session
     "R" 'lsp-find-references
     )
    )
  )

(use-package typescript-mode
  :ensure t
  :init
    (add-to-list 'auto-mode-alist '("\\.ts\\'" . typescript-mode))
    (add-hook 'typescript-mode-hook #'lsp))

(provide 'setup-programming)
;;; setup-programming.el ends here
