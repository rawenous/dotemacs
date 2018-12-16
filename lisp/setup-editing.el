;;; Package --- Summary
;;; Commentary:
;;; Code:


(column-number-mode 1) ;; show column number in modeline
(winner-mode 1) ;; Enable saving of window configuration
(add-hook 'before-save-hook 'whitespace-cleanup)
(blink-cursor-mode -1)
(if (fboundp 'scroll-bar-mode) (scroll-bar-mode -1))
(size-indication-mode 1) ;; see buffer size in modeline
(global-hl-line-mode)
(setq ibuffer-use-other-window t)

;; whenever you create useless whitespace, the whitespace is highlighted
(add-hook 'prog-mode-hook (lambda () (interactive) (setq show-trailing-whitespace t)))

(windmove-default-keybindings)

(defun unix-werase-or-kill (arg)
  "Kill marked region. If no marked region do 'backward-kill-word'."
  (interactive "*p")
  (if (and transient-mark-mode
	   mark-active)
      (kill-region (region-beginning) (region-end))
    (backward-kill-word arg)))

(defun prelude-move-beginning-of-line (arg)
  "Move point back to indentation of beginning of line.
Move point to the first non-whitespace character on this line.
If point is already there, move to the beginning of the line.
Effectively toggle between the first non-whitespace character and
the beginning of the line.
If ARG is not nil or 1, move forward ARG - 1 lines first. If
point reaches the beginning or end of the buffer, stop there."
  (interactive "^p")
  (setq arg (or arg 1))

  ;; Move lines first
  (when (/= arg 1)
    (let ((line-move-visual nil))
      (forward-line (1- arg))))

  (let ((orig-point (point)))
    (back-to-indentation)
    (when (= orig-point (point))
      (move-beginning-of-line 1))))




(provide 'setup-editing)
;;; setup-editing.el ends here
