;;; Package --- Summary
;;; Commentary:
;;; Code:
(add-to-list 'load-path "~/.emacs.d/lisp/")

(require 'core)
(require 'setup-editing)
(require 'setup-keybindings)

(use-package swiper :ensure t
  :commands (swiper))

(use-package counsel :ensure t
  :commands (counsel-M-x))

(use-package which-key :ensure t
  :diminish t
  :init
  (which-key-mode))

;; duplicate current line
(use-package duplicate-thing
  :ensure t
  :commands (duplicate-thing))

(use-package doom-themes :ensure t
  :init
  (load-theme 'doom-one t))

(use-package rainbow-mode :ensure t
  :diminish t
  :init
  (rainbow-mode))

;; highlight text that was inserted
(use-package volatile-highlights
  :ensure t
  :diminish volatile-highlights-mode
  :config
(volatile-highlights-mode t))

(use-package rainbow-delimiters :ensure t
  :init
  :config
  (add-hook 'prog-mode-hook 'rainbow-delimiters-mode))

;; Hihlight the parentheses that surrounds the cursor
(use-package highlight-parentheses
  :ensure t
  :config
  (add-hook 'prog-mode-hook 'highlight-parentheses-mode)
  (set-face-attribute 'hl-paren-face nil :weight 'ultra-bold)
  :diminish highlight-parentheses-mode
  )


(use-package doom-modeline
  :ensure t
  :defer t
  :hook (after-init . doom-modeline-init))

;; Company is a text completion framework for Emacs. The name stands for "complete anything".
(use-package company
  :ensure t
  :config
  (global-company-mode)
  :diminish company-mode)

(use-package magit
  :ensure t
  :config
  (add-hook 'magit-mode-hook 'magit-load-config-extensions))

(use-package git-timemachine
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
  (global-git-gutter-mode t))

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

(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(package-selected-packages
   (quote
    (expand-region highlight-numbers duplicate-thing volatile-highlights highlight-parentheses multiple-cursors mutiple-cursors git-gutter flycheck git-timemachine magit company doom-modeline rainbow-delimiters rainbow-delimeters rainbow-mode doom-themes which-key counsel swiper avy general use-package))))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )

(provide 'init)
;;; init.el ends here
