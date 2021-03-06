;;; Package --- Summary
;;; Commentary:
;;; Code:

;; Reverse behavior of helm-grep-git-1 to grep whole repo instead of
;; current dir
(defun my-helm-grep-do-git-grep (not-all)
  (interactive "P")
  (helm-grep-git-1 default-directory (null not-all)))

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

(use-package projectile
  :ensure t
  :init
  (projectile-mode)
  (helm-projectile-on)
  :config
  (setq projectile-completion-system 'helm
        projectile-enable-caching nil
        )
  )

(use-package helm-projectile
  :ensure t
  :init
  )

(use-package helm-swoop
  :ensure t
  :commands (helm-swoop))

(provide 'setup-helm)
;;; setup-helm.el ends here
