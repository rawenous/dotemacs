;;; Package --- Summary
;;; Commentary:
;;; Code:

;;; Setup javascript
(defun ladan/set-in-project-eslint ()
  "Set eslint executable to the one in node_modules."
  (setq current-dir default-directory)
  (while (equal nil (let* ((root (locate-dominating-file current-dir "node_modules"))
                           (eslint (and root
                                        (expand-file-name "node_modules/eslint/bin/eslint.js"
                                                          root))))
                      (when (and eslint (file-executable-p eslint))
                        (setq-local flycheck-javascript-eslint-executable eslint)
                        (setq-local flycheck-eslint-args '("--cache-location" "~/.cache/.eslintcache" "--cache")
                                    ))))
    (setq current-dir (concat (locate-dominating-file current-dir "node_modules") "../"))
    )
  )

(use-package js2-mode
  :ensure t
  :init
    (add-to-list 'auto-mode-alist '("\\.js\\'" . js2-mode))
    (add-hook 'js2-mode-hook #'lsp)
    (add-hook 'flycheck-mode-hook #'ladan/set-in-project-eslint)
  :config
  (setq js-indent-level 2
        js2-mode-show-parse-errors nil
        js2-mode-show-strict-warnings nil)
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
