;;; Package --- Summary
;;; Commentary:
;;; Code:

;; Lets make M-m my prefix key as in spacemacs
;; It was bound to 'back-to-indentation'
(global-unset-key (kbd "M-m"))

(use-package general :ensure t
  :config
  (general-define-key "C-'" 'avy-goto-word-1)
  )

(use-package avy :ensure t
  :commands (avy-goto-word-1))

(general-define-key
 ;; replace default keybindings
 "C-s" 'swiper
 "M-x" 'counsel-M-x         ; replace default M-x with ivy backend
 "C-x b" 'ivy-switch-buffer ; change, buffer chosing ivy
 "M-/" 'hippie-expand       ; replace dabbrev-expand
 "C-x C-f" 'counsel-find-file

 ;; Multiple-cursors rocks!
 "C-S-c C-S-c" 'mc/edit-lines
 "C->" 'mc/mark-next-like-this
 "C-<" 'mc/mark-previous-like-this
 "C-c C-<" 'mc/mark-all-like-this

 "M-c" 'duplicate-thing ;; duplicate current line
 "C-a" 'prelude-move-beginning-of-line ; smarter beginning of line
 "C-w" 'unix-werase-or-kill ; same behaviour as in terminal
 "M-n" 'er/expand-region

 ;; "M-y" 'helm-show-kill-ring
 )

(general-define-key
 :prefix "M-m"
 ;; bind to simple key press
 ;;"b"	'ivy-switch-buffer  ; change buffer, chose using ivy
 "TAB" '(spacemacs/alternate-buffer :which-key "last buffer")
 "b" 'counsel-ibuffer
  "/"   'counsel-git-grep   ; find string in git project
  "*" 'counsel-ag           ; use ag for general search
  ;; file stuff
  "f"   '(:ignore t :which-key "files")
  "ff"  'counsel-find-file ; find file using ivy
  "fr"  'counsel-recentf    ; find recent files

  ;; Project stuff
  "p"   '(:ignore t :which-key "project")
  "pf" 'counsel-git
  ;; "p" '(projectile-command-map :which-key "project")

  ;; Searching high and low
  "s"   '(:ignore t :which-key "searching")
  "ss"  'swiper             ; search for string in current buffer

  ;; Jumping around
  "j"   '(:ignore t :which-key "jumping")
  "jj"  'avy-goto-char-timer ; When you stop typing that word will match
  "ji"  'counsel-imenu

  ;; Toggles
  "t"   '(:ignore t :which-key "toggles")
  "tw"  'whitespace-mode

  ;; Git commands but only magit forn ow
  "g"   '(:ignore t :which-key "git")
  "gfh" 'magit-log-buffer-file
  "gm"  'magit-dispatch-popup
  "gs"  'magit-status
  "gt"  'git-timemachine
  "gS"  'magit-stage-file
  "gU"  'magit-unstage-file
  )

(provide 'setup-keybindings)
;;; setup-keybindings.el ends here
