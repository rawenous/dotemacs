;;; Package --- Summary
;;; Commentary:
;;; Code:

;; Lets make M-m my prefix key as in spacemacs
;; It was bound to 'back-to-indentation'
(global-unset-key (kbd "M-m"))


;; Awesome. Like hungry delete but better
(define-key global-map [remap cycle-spacing] (lambda () (interactive) (cycle-spacing -1)))
(define-key minibuffer-inactive-mode-map
            (kbd "C-l") #'up-directory)
(define-key isearch-mode-map
            (kbd "C-l") #'up-directory)

(define-key minibuffer-local-filename-completion-map
            [C-l] #'up-directory)

(use-package avy :ensure t
 :commands (avy-goto-word-1))

(general-define-key
 "M-/" 'hippie-expand       ; replace dabbrev-expand

 "C-a" 'prelude-move-beginning-of-line ; smarter beginning of line
 "C-w" 'unix-werase-or-kill ; same behaviour as in terminal
 "M-n" 'er/expand-region

 "C-c t" 'todo-show

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

  "a"   '(:ignore t :which-key "applications")

  "e"    '(:ignore t :which-key "errors")

  "x"    '(:ignore t :which-key "text")
  "xl"    '(:ignore t :which-key "lines")
  "xld" '(ravenous/duplicate-line-or-region :which-key "duplicate-line-or-region")

  ;; Searching high and low
  "s"   '(:ignore t :which-key "searching")

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
