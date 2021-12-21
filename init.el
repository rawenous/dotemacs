;;; Package --- Summary
;;; Commentary:
;;; Code:
(add-to-list 'load-path "~/.emacs.d/lisp/")

(require 'setup-core)
(require 'setup-programming)
(require 'setup-editing)
(require 'setup-keybindings)
;; (require 'setup-helm)

(setq explicit-shell-file-name "/bin/zsh")
(setq shell-file-name "zsh")

;; Use PATH from shell
(use-package exec-path-from-shell :ensure t
  :init
  (when (memq window-system '(mac ns x))
  (exec-path-from-shell-initialize))
  )

;; (use-package swiper :ensure t
;;   :commands (swiper)
;;   :config
;;   (bind-key "M-c" #'swiper-mc swiper-map))

;; (use-package counsel :ensure t
;;   :commands (counsel-M-x)
;;   :init (counsel-mode)
;;   :config
;;   (setq ivy-use-virtual-buffers t)
;;   :general
;;   (my-leader-def "/" 'counsel-git-grep)
;;   )

(use-package add-node-modules-path
  :hook js2-mode)


;; (use-package php-mode
;;   :ensure t
;;   :config
;;   (with-eval-after-load "lsp"
;;     (assq-delete-all 'web-mode lsp-language-id-configuration)
;;     (add-to-list 'lsp-language-id-configuration '(web-mode . "php")))

;;   ;; Run web-mode with php lsp when opening php files
;;   (add-to-list 'auto-mode-alist '("\\.php\\'" . web-mode))
;;   (add-hook 'web-mode-hook #'lsp)
;;   (setq web-mode-code-indent-offset 2)
;;   )

(use-package rjsx-mode
  :mode "\\.jsx"
  :defer t
  :init
  (add-to-list 'magic-mode-alist (cons #'ravenous/javascript-jsx-file-p 'rjsx-mode))
  )

(use-package all-the-icons
  :ensure t)

(use-package treemacs
  :general
  (my-leader-def
    "at" 'treemacs)
  :config
  (treemacs-follow-mode t)
  (treemacs-filewatch-mode t))

(use-package treemacs-projectile
  :after (treemacs projectile))

(use-package all-the-icons-dired
  :hook (dired-mode . all-the-icons-dired-mode)
  )

(use-package which-key
  :ensure t
  :config (which-key-mode)
  :diminish t
  )

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
  :commands rainbow-delimiters-mode
  :init
  (add-hook 'prog-mode-hook #'rainbow-delimiters-mode))

;; Hihlight the parentheses that surrounds the cursor
(use-package highlight-parentheses
  :hook (prog-mode . highlight-parentheses-mode)
  ;; :config
  ;; (add-hook 'prog-mode-hook 'highlight-parentheses-mode)
  ;; :diminish highlight-parentheses-mode
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
  :config
  (setq projectile-completion-system 'default
        projectile-enable-caching nil
        projectile-indexing-method 'hybrid
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
  :init
    (progn
    (define-fringe-bitmap 'my-flycheck-fringe-indicator
      (vector #b00000000
              #b00000000
              #b00000000
              #b00000000
              #b00000000
              #b00000000
              #b00000000
              #b00011100
              #b00111110
              #b00111110
              #b00111110
              #b00011100
              #b00000000
              #b00000000
              #b00000000
              #b00000000
              #b00000000))

    (flycheck-define-error-level 'error
      :severity 2
      :overlay-category 'flycheck-error-overlay
      :fringe-bitmap 'my-flycheck-fringe-indicator
      :fringe-face 'flycheck-fringe-error)

    (flycheck-define-error-level 'warning
      :severity 1
      :overlay-category 'flycheck-warning-overlay
      :fringe-bitmap 'my-flycheck-fringe-indicator
      :fringe-face 'flycheck-fringe-warning)

    (flycheck-define-error-level 'info
      :severity 0
      :overlay-category 'flycheck-info-overlay
      :fringe-bitmap 'my-flycheck-fringe-indicator
      :fringe-face 'flycheck-fringe-info))
  :config
  (add-hook 'prog-mode-hook 'flycheck-mode)
  :general
  (my-leader-def
    "e l" 'ravenous/open-or-close-flycheck-errors
    "e v" 'flycheck-verify-setup
    "e n" 'flycheck-next-error
    "e n" 'flycheck-previous-error
    ))

(use-package git-gutter
  :ensure t
  :init
;;  (setq git-gutter:update-interval 2)
)

(use-package multiple-cursors
  :ensure t
  ;;:commands mc/mark-all-dwim
  :bind (
        ("C-S-c C-S-c". mc/edit-lines)
        ("C->" . mc/mark-next-like-this)
        ("C-<" . mc/mark-previous-like-this)
        ("C-c C-<" . mc/mark-all-like-this)
        )
)

(use-package highlight-numbers
  :ensure t
  :init (highlight-numbers-mode t)
  )

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

;; (use-package ivy-hydra
;;   :ensure t)

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

(use-package prettier
  :ensure t)

(use-package lsp-mode
  :ensure t
  :commands lsp
  :config
  (add-hook 'c-mode-hook #'lsp)
  (setq
   lsp-enable-indentation nil
   lsp-prefer-flymake :none)
)

(use-package yaml-mode
  :ensure t
  :mode ("\\.yaml\\'" "\\.yml\\'")
  )

(use-package lsp-ui :ensure t :commands lsp-ui-mode
  :config
  (setq lsp-ui-sideline-enable nil)
  )

(use-package smartparens
  :ensure t
  :hook (prog-mode . smartparens-mode)
)

(use-package json-mode
  :ensure t
  :mode "\\.json\\'")

(use-package hl-todo
  :ensure t
  :config
  (add-hook 'prog-mode-hook #'hl-todo-mode))

(use-package visual-regexp-steroids
  :commands vr/query-replace
)

;; Some completion alternatives
(use-package selectrum
  :ensure t
  :init
  (selectrum-mode +1)
  :config
  (define-key selectrum-minibuffer-map
    (kbd "C-l") #'up-directory)

  )

(use-package selectrum-prescient
  :ensure t
  :init
   (selectrum-prescient-mode +1)
   (prescient-persist-mode +1)
   )

(use-package consult
  :ensure t
  :bind (
         ("C-x C-b" . consult-buffer)
         ("C-x b" . consult-buffer)
         ("M-y" . consult-yank-pop)                ;; orig. yank-pop
         ("<help> a" . consult-apropos)            ;; orig. apropos-command
         ("M-g g" . consult-goto-line)             ;; orig. goto-line
         ("M-g M-g" . consult-goto-line)             ;; orig. goto-line
         ;; M-s bindings (search-map)
         ("M-s f" . consult-find)
         ("M-s F" . consult-locate)
         ("M-s g" . consult-grep)
         ("M-s G" . consult-git-grep)
         ("M-s r" . consult-ripgrep)
         ("M-s l" . consult-line)
         ("M-s L" . consult-line-multi)
         ("M-s m" . consult-multi-occur)
         ("M-s k" . consult-keep-lines)
         ("M-s u" . consult-focus-lines)
         ("M-s e" . consult-isearch-history)
         ("M-s j" . consult-find-from-minibuffer)
         :map isearch-mode-map
         ("M-e" . consult-isearch-history)         ;; orig. isearch-edit-string
         ("M-s e" . consult-isearch-history)       ;; orig. isearch-edit-string
         ("M-s l" . consult-line)                  ;; needed by consult-line to detect isearch
         ("M-s L" . consult-line-multi))           ;; needed by consult-line to detect isearch

  :general
  (my-leader-def
    "ss" 'consult-line
    "sS" 'consult-line-symbol-at-point
    "sk" 'consult-keep-lines
    "ji" 'consult-imenu
    "/" '(consult-ripgrep :which-key "project search")
    )

  :config
  ;; Optionally configure the narrowing key.
  ;; Both < and C-+ work reasonably well.
  (setq consult-narrow-key "<") ;; (kbd "C-+")

  (autoload 'projectile-project-root "projectile")
  (setq consult-project-root-function #'projectile-project-root)

  ;; Not working yet :(
  (defun consult-find-from-minibuffer (path)
    "Grep from current minibuffer directory."
    (interactive "p")
    (if (minibufferp (current-buffer))
        (print-rickard (minibuffer-contents))
      (message "This command can only be run from the minibuffer")
      ))

  (defun print-rickard (directory)
    (consult-ripgrep directory)
    (abort-recursive-edit))

  (defun consult-line-symbol-at-point ()
    (interactive)
    (consult-line (thing-at-point 'symbol)))
  )

;; Enable richer annotations using the Marginalia package
(use-package marginalia
  ;; Either bind `marginalia-cycle` globally or only in the minibuffer
  :bind (:map minibuffer-local-map
         ("M-A" . marginalia-cycle))

  ;; The :init configuration is always executed (Not lazy!)
  :init

  ;; Must be in the :init section of use-package such that the mode gets
  ;; enabled right away. Note that this forces loading the package.
  (marginalia-mode))


(use-package undo-tree
  :init
  (global-undo-tree-mode 1)
  :commands undo-tree-visualize
  :general
  (my-leader-def
    "a u" 'undo-tree-visualize
    ))

(defun ravenous/my-restart-resume-layouts ()
  "Restart Emacs with resumed layouts."
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
 '(ansi-color-names-vector
   ["#282c34" "#ff6c6b" "#98be65" "#ECBE7B" "#51afef" "#c678dd" "#46D9FF" "#bbc2cf"])
 '(custom-safe-themes
   '("835868dcd17131ba8b9619d14c67c127aa18b90a82438c8613586331129dda63" "1704976a1797342a1b4ea7a75bdbb3be1569f4619134341bd5a4c1cfb16abad4" "d268b67e0935b9ebc427cad88ded41e875abfcc27abd409726a92e55459e0d01" "4b0e826f58b39e2ce2829fab8ca999bcdc076dec35187bf4e9a4b938cb5771dc" "da53441eb1a2a6c50217ee685a850c259e9974a8fa60e899d393040b4b8cc922" "fe2539ccf78f28c519541e37dc77115c6c7c2efcec18b970b16e4a4d2cd9891d" "7a7b1d475b42c1a0b61f3b1d1225dd249ffa1abb1b7f726aec59ac7ca3bf4dae" "d2e9c7e31e574bf38f4b0fb927aaff20c1e5f92f72001102758005e53d77b8c9" "6d589ac0e52375d311afaa745205abb6ccb3b21f6ba037104d71111e7e76a3fc" "6b2636879127bf6124ce541b1b2824800afc49c6ccd65439d6eb987dbf200c36" "f0dc4ddca147f3c7b1c7397141b888562a48d9888f1595d69572db73be99a024" default))
 '(debug-on-error nil)
 '(exwm-floating-border-color "#191b20")
 '(fci-rule-color "#5B6268")
 '(highlight-tail-colors
   ((("#333a38" "#99bb66" "green")
     . 0)
    (("#2b3d48" "#46D9FF" "brightcyan")
     . 20)))
 '(jdee-db-active-breakpoint-face-colors (cons "#1B2229" "#51afef"))
 '(jdee-db-requested-breakpoint-face-colors (cons "#1B2229" "#98be65"))
 '(jdee-db-spec-breakpoint-face-colors (cons "#1B2229" "#3f444a"))
 '(objed-cursor-color "#ff6c6b")
 '(package-selected-packages
   '(php-mode embark-consult mini-frame marginalia consult selectrum clang-format selectrum-prescient tree-sitter-langs tree-sitter treemacs-projectile treemacs all-the-icons-dired rjsx-mode add-node-modules-path undo-tree shell-pop restart-emacs ivy-posframe visual-regexp-steroids prettier js-doc hl-todo web-mode json-mode typescript-mode smartparens yaml-mode exec-path-from-shell company-lsp lsp-ui lsp-mode ivy-hydra hydra xref-js2 js2-refactor js2-mode winum eyebrowse expand-region highlight-numbers duplicate-thing volatile-highlights highlight-parentheses multiple-cursors mutiple-cursors git-gutter flycheck git-timemachine magit company doom-modeline rainbow-delimiters rainbow-delimeters rainbow-mode doom-themes which-key counsel swiper avy general use-package))
 '(pdf-view-midnight-colors (cons "#bbc2cf" "#282c34"))
 '(rcirc-server-alist '(("irc.libera.chat" :channels ("#emacs"))))
 '(rustic-ansi-faces
   ["#282c34" "#ff6c6b" "#98be65" "#ECBE7B" "#51afef" "#c678dd" "#46D9FF" "#bbc2cf"])
 '(safe-local-variable-values
   '((eval ignore-errors
           (require 'whitespace)
           (whitespace-mode 0)
           (whitespace-mode 1))
     (eval prettier-mode t)))
 '(vc-annotate-background "#282c34")
 '(vc-annotate-color-map
   (list
    (cons 20 "#98be65")
    (cons 40 "#b4be6c")
    (cons 60 "#d0be73")
    (cons 80 "#ECBE7B")
    (cons 100 "#e6ab6a")
    (cons 120 "#e09859")
    (cons 140 "#da8548")
    (cons 160 "#d38079")
    (cons 180 "#cc7cab")
    (cons 200 "#c678dd")
    (cons 220 "#d974b7")
    (cons 240 "#ec7091")
    (cons 260 "#ff6c6b")
    (cons 280 "#cf6162")
    (cons 300 "#9f585a")
    (cons 320 "#6f4e52")
    (cons 340 "#5B6268")
    (cons 360 "#5B6268")))
 '(vc-annotate-very-old-color nil)
 '(volatile-highlights-mode t))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )

(provide 'init)
;;; init.el ends here
