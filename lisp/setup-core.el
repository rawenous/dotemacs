;;; Package --- Summary
;;; Commentary:
;;; Code:
(require 'package)

(setq package-enable-at-startup nil) ; tells emacs not to load any packages before starting up
;; the following lines tell emacs where on the internet to look up
;; for new packages.
(setq package-archives '(("org"       . "http://orgmode.org/elpa/")
                         ("gnu"       . "http://elpa.gnu.org/packages/")
                         ("melpa"     . "https://melpa.org/packages/")))
(package-initialize) ; guess what this one does ?

;; Bootstrap `use-package'
(unless (package-installed-p 'use-package) ; unless it is already installed
  (package-refresh-contents) ; updage packages archive
  (package-install 'use-package)) ; and install the most recent version of use-package

(require 'use-package) ; guess what this one does too ?
(setq use-package-always-ensure t)


(setq delete-old-versions -1 )          ; delete excess backup versions silently
(setq version-control t )               ; use version control
(setq vc-make-backup-files t )          ; make backups file even when in version controlled dir
(setq vc-follow-symlinks t )                                   ; don't ask for confirmation when opening symlinked file
(setq inhibit-startup-screen t )        ; inhibit useless and old-school startup screen
(setq ring-bell-function 'ignore )      ; silent bell when you make a mistake
(setq coding-system-for-read 'utf-8 )   ; use utf-8 by default
(setq coding-system-for-write 'utf-8 )
(setq sentence-end-double-space nil)    ; sentence SHOULD end with only a point.
(setq fill-column 80)                   ; toggle wrapping text at the 80th character
(setq initial-scratch-message "Welcome in Emacs") ; print a default message in the empty scratch buffer opened at startup
(setq-default indent-tabs-mode nil)       ; Use spaces for indenting
(menu-bar-mode -1)                ; disable menu bar
(tool-bar-mode -1)                ; disable tool bar
(recentf-mode +1)
(setq ediff-window-setup-function 'ediff-setup-windows-plain)

(setq org-log-done t)
(setq org-agenda-files (list "~/Documents/work.org"))
(setq org-duration-format (quote h:mm))


;; Rebind yes-no to y-n
(defalias 'yes-or-no-p 'y-or-n-p)

(defun path-join (root &rest dirs)
  "Join paths together starting at ROOT and proceeding with DIRS.
Ex: (path-join \"/tmp\" \"a\" \"b\" \"c\") => /tmp/a/b/c"
  (if (not dirs)
      root
    (apply 'path-join
           (expand-file-name (car dirs) root)
           (cdr dirs))))

(defun spacemacs/alternate-buffer (&optional window)
  "Switch back and forth between current and last buffer in the
current window."
  (interactive)
  (let ((current-buffer (window-buffer window))
        (buffer-predicate
         (frame-parameter (window-frame window) 'buffer-predicate)))
    ;; switch to first buffer previously shown in this window that matches
    ;; frame-parameter `buffer-predicate'
    (switch-to-buffer
     (or (cl-find-if (lambda (buffer)
                       (and (not (eq buffer current-buffer))
                            (or (null buffer-predicate)
                                (funcall buffer-predicate buffer))))
                     (mapcar #'car (window-prev-buffers window)))
         ;; `other-buffer' honors `buffer-predicate' so no need to filter
(other-buffer current-buffer t)))))

(defconst *user-cache-directory*
  (path-join "~" ".cache")
    "Path to user's local cache store.")


;; Setup auto save directory
(defconst *user-auto-save-directory* (path-join *user-cache-directory* "auto-saves"))
;; Setup auto-save-list directory
(defconst *user-auto-save-list-directory* (path-join *user-cache-directory* "auto-saves-list"))

;; Emacs will create the backup dir automatically, but not the autosaves dir.
(make-directory *user-auto-save-directory* t)
(make-directory *user-auto-save-list-directory* t)

(setq
 make-backup-files t        ; backup a file the first time it is saved
 backup-directory-alist `((".*" . , *user-auto-save-directory*)) ; save backup files in ~/.cache/auto-saves/
 auto-save-file-name-transforms `((".*" , *user-auto-save-list-directory* t)) ;transform backups file name
 backup-by-copying t     ; copy the current file into backup directory
 version-control t   ; version numbers for backup files
 delete-old-versions t   ; delete unnecessary versions
 kept-old-versions 6     ; oldest versions to keep when a new numbered backup is made (default: 2)
 kept-new-versions 9 ; newest versions to keep when a new numbered backup is made (default: 2)
 ;; auto-save-default t ; auto-save every buffer that visits a file
 ;; auto-save-timeout 20 ; number of seconds idle time before auto-save (default: 30)
 ;; auto-save-interval 200 ; number of keystrokes between auto-saves (default: 300)
)

;; start emacs maximized
(add-hook 'emacs-startup-hook 'toggle-frame-maximized)

(use-package general :ensure t
  :config
  (general-create-definer my-leader-def
    :prefix "M-m")
  )


;; set up tramp files
(setq
 ;; Persistency files.
 tramp-persistency-file-name (path-join *user-cache-directory* "tramp")
 ;; Auto save storage.
 tramp-auto-save-directory (path-join *user-auto-save-directory* "tramp"))


;; https://www.emacswiki.org/emacs/CopyingWholeLines#h5o-12
(defun ravenous/duplicate-line-or-region (&optional n)
  "Duplicate current line, or region if active.
    With argument N, make N copies.
    With negative N, comment out original line and use the absolute value."
  (interactive "*p")
  (let ((use-region (use-region-p)))
    (save-excursion
      (let ((text (if use-region        ;Get region if active, otherwise line
                      (buffer-substring (region-beginning) (region-end))
                    (prog1 (thing-at-point 'line)
                      (end-of-line)
                      (if (< 0 (forward-line 1)) ;Go to beginning of next line, or make a new one
                          (newline))))))
        (dotimes (i (abs (or n 1)))     ;Insert N times, or once if not specified
          (insert text))))
    (if use-region nil                  ;Only if we're working with a line (not a region)
      (let ((pos (- (point) (line-beginning-position)))) ;Save column
        (if (> 0 n)                             ;Comment out original with negative arg
            (comment-region (line-beginning-position) (line-end-position)))
        (forward-line 1)
        (forward-char pos)))))

(defun ravenous/open-or-close-flycheck-errors ()
  "Opens flycheck errors or closes them if already open"
  (interactive)
  (if (get-buffer "*Flycheck errors*")
      (kill-buffer "*Flycheck errors*")
    (flycheck-list-errors)))


;; Thank you spacemacs!
(defun ravenous/javascript-jsx-file-p ()
  "Enable rjsx mode by using magic-mode-alist."
  (when buffer-file-name
    (and (member (file-name-extension buffer-file-name) '("js" "jsx"))
         (re-search-forward "\\(^\\s-*import React\\|\\( from \\|require(\\)[\"']react\\)"
                            magic-mode-regexp-match-limit t)
         (save-excursion
           (goto-char (match-beginning 1))
           (let ((sexp (syntax-ppss)))
             ;; not inside string or comment
             (not (or (nth 3 sexp)
                      (nth 4 sexp))))))))

(defun efs/display-startup-time ()
  (message "Emacs loaded in %s with %d garbage collections."
           (format "%.2f seconds"
                   (float-time
                   (time-subtract after-init-time before-init-time)))
           gcs-done))

(add-hook 'emacs-startup-hook #'efs/display-startup-time)


(defun up-directory (path)
  "Move up a directory in PATH without affecting the kill buffer."
  (interactive "p")
  (if (string-match-p "/." (minibuffer-contents))
      (let ((end (point)))
            (re-search-backward "/.")
            (forward-char)
            (delete-region (point) end))))

(provide 'setup-core)
;;; core.el ends here
