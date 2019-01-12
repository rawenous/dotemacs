;;; Package --- Summary
;;; Commentary:
;;; Code:

(use-package helm
  :ensure t
  :diminish helm-mode
  :init
  (helm-mode 1)
  (helm-autoresize-mode t)
  :config
  (setq helm-split-window-inside-p           t ; open helm buffer inside current window, not occupy whole other window
	helm-move-to-line-cycle-in-source     t ; move to end or beginning of source when reaching top or bottom of source.
	helm-scroll-amount                    8 ; scroll 8 lines other window using M-<next>/M-<prior>
))

(use-package helm-projectile
  :ensure t
  :init
  (helm-projectile-on)
  )

(use-package helm-swoop
  :ensure t
  :commands (helm-swoop))

(provide 'setup-helm)
;;; setup-helm.el ends here
