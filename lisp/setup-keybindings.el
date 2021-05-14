;;; Package --- Summary
;;; Commentary:
;;; Code:

;; Lets make M-m my prefix key as in spacemacs
;; It was bound to 'back-to-indentation'
(global-unset-key (kbd "M-m"))


;; Awesome. Like hungry delete but better
 (define-key global-map [remap cycle-spacing] (lambda () (interactive) (cycle-spacing -1)))

(use-package avy :ensure t
 :commands (avy-goto-word-1))

(general-define-key
 "M-/" 'hippie-expand       ; replace dabbrev-expand

 ;; Multiple-cursors rocks!
 "C-S-c C-S-c" 'mc/edit-lines
 "C->" 'mc/mark-next-like-this
 "C-<" 'mc/mark-previous-like-this
 "C-c C-<" 'mc/mark-all-like-this

 "C-a" 'prelude-move-beginning-of-line ; smarter beginning of line
 "C-w" 'unix-werase-or-kill ; same behaviour as in terminal
 "M-n" 'er/expand-region

 "M-y" 'helm-show-kill-ring
 "C-c t" 'todo-show
 "C-x C-b" 'helm-buffers-list

 ;; visual-regexp-steroids
 "C-M-%" 'vr/query-replace
 "M-a" 'backward-paragraph
 "M-e" 'forward-paragraph

 ;; org-mode
 "C-c a" 'org-agenda
 )

(my-leader-def
 ;; bind to simple key press
  "TAB" '(spacemacs/alternate-buffer :which-key "last buffer")

    ;; Searching high and low
  "a"   '(:ignore t :which-key "applications")

  ;; Searching high and low
  "s"   '(:ignore t :which-key "searching")
  "ss"  'helm-swoop-without-pre-input             ; search for string in current buffer
  "sS"  'helm-swoop

  ;; Jumping around
  "j"   '(:ignore t :which-key "jumping")
  "jj"  'avy-goto-char-timer ; When you stop typing that word will match

  ;; Toggles
  "t"   '(:ignore t :which-key "toggles")
  "tw"  'whitespace-mode
  "tg"  'global-git-gutter-mode
  )

(provide 'setup-keybindings)
;;; setup-keybindings.el ends here
