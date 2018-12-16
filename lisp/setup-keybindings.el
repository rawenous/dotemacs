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
 "M-x" 'counsel-M-x         ; replace default M-x with ivy backend
 "C-x b" 'ivy-switch-buffer ; change, buffer chosing ivy
 "M-/" 'hippie-expand       ; replace dabbrev-expand

 ;; Multiple-cursors rocks!
 "C-S-c C-S-c" 'mc/edit-lines
 "C->" 'mc/mark-next-like-this
 "C-<" 'mc/mark-previous-like-this
 "C-c C-<" 'mc/mark-all-like-this

 "M-c" 'duplicate-thing ;; duplicate current line
 "C-a" 'prelude-move-beginning-of-line ; smarter beginning of line
 "C-w" 'unix-werase-or-kill ; same behaviour as in terminal
 "M-n" 'er/expand-region
 )

(general-define-key
 :prefix "M-m"
 ;; bind to simple key press
  "b"	'ivy-switch-buffer  ; change buffer, chose using ivy
  "/"   'counsel-git-grep   ; find string in git project

  ;; file stuff
  "f"   '(:ignore t :which-key "files")
  "ff"  'counsel-find-file  ; find file using ivy
  "fr"	'counsel-recentf    ; find recently edited files

  ;; Project stuff
  "p"   '(:ignore t :which-key "project")
  "pf"  'counsel-git        ; find file in git project

  ;; Searching high and low
  "s"   '(:ignore t :which-key "searching")
  "ss"  'swiper             ; search for string in current buffer

  ;; Jumping around
  "j"   '(:ignore t :which-key "jumping")
  "jj"  'avy-goto-char-timer ; When you stop typing that word will match

  ;; Toggles
  "T"   '(:ignore t :which-key "toggles")
  "Tw"  'whitespace-mode

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
