;;; Package --- Summary
;;; Commentary:
;;; Code:

;; Reverse behavior of helm-grep-git-1 to grep whole repo instead of
;; current dir
(defun my-helm-grep-do-git-grep (not-all)
  (interactive "P")
  (helm-grep-git-1 default-directory (null not-all)))

(use-package helm
  :diminish helm-mode
  :init
  (helm-mode 1)
  (helm-autoresize-mode nil)
  :config
  (setq helm-split-window-inside-p           t ; open helm buffer
                                               ; inside current
                                               ; window, not occupy
                                               ; whole other window
        helm-move-to-line-cycle-in-source     t ; move to end or
                                                ; beginning of source
                                                ; when reaching top or
                                                ; bottom of source.
        helm-scroll-amount                    8 ; scroll 8 lines other
                                                ; window using
                                        ; M-<next>/M-<prior>
        )

  ;; Swithc helm default behaviour
  (define-key helm-map (kbd "TAB") 'helm-execute-persistent-action)
  (define-key helm-map (kbd "C-z") 'helm-select-action)

  :general
  ("M-x" 'helm-M-x)         ; replace default M-x
  ("C-x C-f" 'helm-find-files)
  ("C-x b" 'helm-mini) ; change, buffer chosing ivy
  ("M-y" 'helm-show-kill-ring)
  ("C-x C-b" 'helm-buffers-list)

  (my-leader-def
   "b" '(helm-mini :which-key "buffers")
    "*" 'helm-do-grep-ag
    "/" 'my-helm-grep-do-git-grep ; Modified function to grep whole repo by default
    "f"   '(:ignore t :which-key "files")
    "ff" 'helm-find-files
    "fr" 'helm-recentf
    "ji" 'helm-imenu

    "ss"  'helm-swoop-without-pre-input             ; search for string in current buffer
    "sS"  'helm-swoop
    )
  )

(use-package helm-projectile
  :init
  )

(use-package helm-swoop
  :commands (helm-swoop))

(provide 'setup-helm)
;;; setup-helm.el ends here
