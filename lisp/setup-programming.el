;;; Package --- Summary
;;; Commentary:
;;; Code:

(defhydra js2-refactor-hydra (:color blue :hint nil)
    "
^Functions^                    ^Variables^               ^Buffer^                      ^sexp^               ^Debugging^
------------------------------------------------------------------------------------------------------------------------------
[_lp_] Localize Parameter      [_ev_] Extract variable   [_wi_] Wrap buffer in IIFE    [_k_]  js2 kill      [_lt_] log this
[_ef_] Extract function        [_iv_] Inline variable    [_ig_] Inject global in IIFE  [_ss_] split string  [_dt_] debug this
[_ip_] Introduce parameter     [_rv_] Rename variable    [_ee_] Expand node at point   [_sl_] forward slurp
[_em_] Extract method          [_vt_] Var to this        [_cc_] Contract node at point [_ba_] forward barf
[_ao_] Arguments to object     [_sv_] Split var decl.    [_uw_] unwrap
[_tf_] Toggle fun exp and decl [_ag_] Add var to globals
[_ta_] Toggle fun expr and =>  [_ti_] Ternary to if
[_q_]  quit"
    ("ee" js2r-expand-node-at-point)
("cc" js2r-contract-node-at-point)
("ef" js2r-extract-function)
("em" js2r-extract-method)
("tf" js2r-toggle-function-expression-and-declaration)
("ta" js2r-toggle-arrow-function-and-expression)
("ip" js2r-introduce-parameter)
("lp" js2r-localize-parameter)
("wi" js2r-wrap-buffer-in-iife)
("ig" js2r-inject-global-in-iife)
("ag" js2r-add-to-globals-annotation)
("ev" js2r-extract-var)
("iv" js2r-inline-var)
("rv" js2r-rename-var)
("vt" js2r-var-to-this)
("ao" js2r-arguments-to-object)
("ti" js2r-ternary-to-if)
("sv" js2r-split-var-declaration)
("ss" js2r-split-string)
("uw" js2r-unwrap)
("lt" js2r-log-this)
("dt" js2r-debug-this)
("sl" js2r-forward-slurp)
("ba" js2r-forward-barf)
("k" js2r-kill)
("q" nil)
)

(defvar current-dir)

;;; Setup javascript
(defun ladan/set-in-project-eslint ()
  "Set eslint executable to the one in node_modules."
  (setq current-dir default-directory)
  (while (and (not (equal current-dir "../")) (equal nil (let* ((root (locate-dominating-file current-dir "node_modules"))
                           (eslint (and root
                                        (expand-file-name "node_modules/eslint/bin/eslint.js"
                                                          root))))
                      (when (and eslint (file-executable-p eslint))
                        (setq-local flycheck-javascript-eslint-executable eslint)
                        (setq-local flycheck-eslint-args '("--cache-location" "~/.cache/.eslintcache" "--cache")
                                    )))))
    (setq current-dir (concat (locate-dominating-file current-dir "node_modules") "../"))
    )
  )

(use-package web-mode
  :ensure t)

(use-package js2-refactor
  :ensure t)

(use-package js-doc
  :ensure t
  :config
  (general-define-key
   :keymaps 'js2-mode-map
   :prefix "C-c c"
   "di" '(js-doc-insert-function-doc :which-key "docs"))
  )

(use-package js2-mode
  :ensure t
  :init
    (add-to-list 'auto-mode-alist '("\\.js\\'" . js2-mode))
    (add-hook 'js2-mode-hook #'lsp)
    (add-hook 'flycheck-mode-hook #'ladan/set-in-project-eslint)
    (add-hook 'js2-mode-hook #'js2-refactor-mode)
  :config
  (setq js-indent-level 2
        js2-mode-show-parse-errors nil
        js2-mode-show-strict-warnings nil)
  (eval-after-load 'lsp-mode
    (general-define-key
     :keymaps 'js2-mode-map
     :prefix "C-c c"
     "r" 'lsp-rename
     "p" 'lsp-ui-peek-find-implementation
     "h" 'lsp-describe-thing-at-point
     "H" 'lsp-describe-session
     "R" 'lsp-find-references
     "e" 'js2-refactor-hydra/body
     "k" 'js2r-kill
     )
    )
  )

(use-package typescript-mode
  :ensure t
  :init
    (add-to-list 'auto-mode-alist '("\\.ts\\'" . typescript-mode))
    (add-hook 'typescript-mode-hook #'lsp))



(provide 'setup-programming)
;;; setup-programming.el ends here
