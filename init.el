;;; Package --- Summary
;;; Commentary:
;;; Code:
(add-to-list 'load-path "~/.emacs.d/lisp/")

(require 'core)
(require 'setup-programming)
(require 'setup-editing)
(require 'setup-keybindings)
(require 'setup-helm)

;; Use PATH from shell
(use-package exec-path-from-shell :ensure t
  :init
  (when (memq window-system '(mac ns x))
  (exec-path-from-shell-initialize))
  )

(use-package swiper :ensure t
  :commands (swiper)
  :config
  (bind-key "M-c" #'swiper-mc swiper-map))

;; (use-package counsel :ensure t
;;   :commands (counsel-M-x)
;;   :init (counsel-mode)
;;   :config
;;   (setq ivy-use-virtual-buffers t)
;;   :general
;;   (my-leader-def "/" 'counsel-git-grep)
;;   )


(use-package which-key
  :diminish t
  :init
  (which-key-mode))

;; duplicate current line
(use-package duplicate-thing
  :ensure t
  :commands (duplicate-thing))

(use-package doom-themes
  :init
  (load-theme 'doom-one t))

(use-package rainbow-mode
  :diminish t
  :init
  (rainbow-mode))

;; highlight text that was inserted
(use-package volatile-highlights
  :diminish volatile-highlights-mode
  :config
  (volatile-highlights-mode t))

(use-package rainbow-delimiters
  :init
  :config
  (add-hook 'prog-mode-hook 'rainbow-delimiters-mode))

;; Hihlight the parentheses that surrounds the cursor
(use-package highlight-parentheses
  :config
  (add-hook 'prog-mode-hook 'highlight-parentheses-mode)
  (set-face-attribute 'hl-paren-face nil :weight 'ultra-bold :box t)
  :diminish highlight-parentheses-mode
  )

(use-package doom-modeline
  :config
  (setq doom-modeline-buffer-file-name-style 'file-name)
  :defer t
  :hook (after-init . doom-modeline-init))

;; Company is a text completion framework for Emacs. The name stands for "complete anything".
(use-package company
  :config
  (global-company-mode)
  :diminish company-mode)

(use-package magit
  :defer t
  :config
  (add-hook 'magit-mode-hook 'magit-load-config-extensions)
  :general
  (my-leader-def
    ;; Git commands but only magit for now
    "g"   '(:ignore t :which-key "git")
    "gb"  'magit-blame
    "gfh" 'magit-log-buffer-file
    "gm"  'magit-dispatch-popup
    "gs"  'magit-status
    "gt"  'git-timemachine
    "gS"  'magit-stage-file
    "gU"  'magit-unstage-file
    )
  )

(use-package projectile
  :defer t
  :ensure t
  :init
  (projectile-mode)
  (helm-projectile-on)
  :config
  (setq projectile-completion-system 'helm
        projectile-enable-caching nil
        )
  :general
  (my-leader-def
    ;; Project stuff
    "p" '(projectile-command-map :which-key "project")
    ))


(use-package git-timemachine
  :defer t
  :ensure t
  :commands (git-timemachine))

(use-package flycheck
  :ensure t
  :config
  (add-hook 'prog-mode-hook 'flycheck-mode))

(use-package git-gutter
  :ensure t
  :init
;;  (setq git-gutter:update-interval 2)
)

(use-package multiple-cursors
  :ensure t
)

(use-package highlight-numbers
  :ensure t
  :config
  (add-hook 'prog-mode-hook 'highlight-numbers-mode))

;; allows you to select text objects incrementally.
(use-package expand-region
  :ensure t
  :commands (er/expand-region)
)

(use-package eyebrowse
  :ensure t
  :init
  (eyebrowse-mode t))

(use-package winum
  :ensure t
  :config
  (progn
    (setq winum-auto-assign-0-to-minibuffer nil
          winum-auto-setup-mode-line nil
          winum-ignored-buffers '(" *which-key*"))
    (define-key winum-keymap (kbd "M-0") 'winum-select-window-0-or-10)
    (define-key winum-keymap (kbd "M-1") 'winum-select-window-1)
    (define-key winum-keymap (kbd "M-2") 'winum-select-window-2)
    (define-key winum-keymap (kbd "M-3") 'winum-select-window-3)
    (define-key winum-keymap (kbd "M-4") 'winum-select-window-4)
    (define-key winum-keymap (kbd "M-5") 'winum-select-window-5)
    (define-key winum-keymap (kbd "M-6") 'winum-select-window-6)
    (define-key winum-keymap (kbd "M-7") 'winum-select-window-7)
    (define-key winum-keymap (kbd "M-8") 'winum-select-window-8)
    (define-key winum-keymap (kbd "M-9") 'winum-select-window-9)
    (winum-mode)))

(use-package hydra
  :ensure t
)

(use-package ivy-hydra
  :ensure t)

;; Not used at the moment
;; (defhydra hydra-move
;;    (:body-pre (next-line))
;;    "move"
;;    ("n" next-line)
;;    ("p" previous-line)
;;    ("f" forward-char)
;;    ("b" backward-char)
;;    ("a" backward-paragraph)
;;    ("e" forward-paragraph)
;;    ("v" scroll-up-command)
;;    ;; Converting M-v to V here by analogy.
;;    ("V" scroll-down-command)
;;    ("l" recenter-top-bottom))


(use-package lsp-mode
  :ensure t
  :commands lsp
  :config
  (setq
   lsp-enable-indentation nil
   lsp-prefer-flymake :none)
)

(use-package yaml-mode
  :ensure t
  )

(use-package lsp-ui :ensure t :commands lsp-ui-mode
  :config
  (setq lsp-ui-sideline-enable nil)
  )
(use-package company-lsp :ensure t :commands company-lsp)

(use-package smartparens
  :ensure t
  :config
  (add-hook 'prog-mode-hook #'smartparens-mode)
  )

(use-package json-mode
  :ensure t)

(use-package hl-todo
  :ensure t
  :config
  (add-hook 'prog-mode-hook #'hl-todo-mode))

(use-package visual-regexp-steroids

)

(use-package undo-tree
  :init
  (undo-tree-mode 1)
  :general
  (my-leader-def
    "a u" 'undo-tree-visualize
    ))

(defun ravenous/my-restart-resume-layouts ()
  "Restart emacs with resumed layouts"
  (interactive)
  (setq restart-emacs-restore-frames t)
  (restart-emacs))

(use-package restart-emacs
  :defer t
  :init
  :general
  (my-leader-def
    "qr" '(ravenous/my-restart-resume-layouts :which-key "resume layouts")
    "qR" '(restart-emacs :which-key "restart")))

(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(custom-safe-themes
   (quote
    ("d2e9c7e31e574bf38f4b0fb927aaff20c1e5f92f72001102758005e53d77b8c9" "6d589ac0e52375d311afaa745205abb6ccb3b21f6ba037104d71111e7e76a3fc" "6b2636879127bf6124ce541b1b2824800afc49c6ccd65439d6eb987dbf200c36" "f0dc4ddca147f3c7b1c7397141b888562a48d9888f1595d69572db73be99a024" default)))
 '(debug-on-error nil)
 '(package-selected-packages
   (quote
    (undo-tree shell-pop restart-emacs ivy-posframe selectrum visual-regexp-steroids prettier js-doc hl-todo web-mode json-mode typescript-mode smartparens yaml-mode exec-path-from-shell company-lsp lsp-ui lsp-mode ivy-hydra hydra xref-js2 js2-refactor js2-mode winum eyebrowse expand-region highlight-numbers duplicate-thing volatile-highlights highlight-parentheses multiple-cursors mutiple-cursors git-gutter flycheck git-timemachine magit company doom-modeline rainbow-delimiters rainbow-delimeters rainbow-mode doom-themes which-key counsel swiper avy general use-package))))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )

(provide 'init)
;;; init.el ends here
