;;; Package --- Summary
;;; Commentary:
;;; Code:
(add-to-list 'load-path "~/.emacs.d/lisp/")

(require 'core)
(require 'setup-keybindings)

(use-package swiper :ensure t
  :commands (swiper))

(use-package counsel :ensure t
  :commands (counsel-M-x))

(use-package which-key :ensure t
  :diminish t
  :init
  (which-key-mode))

(use-package doom-themes :ensure t
  :init
  (load-theme 'doom-one t))

(use-package rainbow-mode :ensure t
  :diminish t
  :init
  (rainbow-mode))

(use-package rainbow-delimiters :ensure t
  :init
  (rainbow-delimiters-mode))

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

(add-hook 'before-save-hook 'whitespace-cleanup)


(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(package-selected-packages
   (quote
    (multiple-cursors mutiple-cursors git-gutter flycheck git-timemachine magit company doom-modeline rainbow-delimiters rainbow-delimeters rainbow-mode doom-themes which-key counsel swiper avy general use-package))))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )

(provide 'init)
;;; init.el ends here
