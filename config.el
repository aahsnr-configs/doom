;;; $DOOMDIR/config.el -*- lexical-binding: t; -*-

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Important Note on Startup Errors (e.g., Treemacs)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; If you are experiencing a startup error like "(wrong-number-of-arguments)"
;; involving `treemacs` and `persp-activate`, this is an internal Doom Emacs
;; package conflict, not an error in this file.
;;
;; To fix it, run the following commands in your terminal and restart Emacs:
;; 1. ~/.emacs.d/bin/doom sync
;; 2. (If the problem persists) ~/.emacs.d/bin/doom upgrade
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; Default theme
(setq doom-theme 'doom-tokyo-night)

;; This determines the style of line numbers in effect.
(setq display-line-numbers-type t)

;; Set the org directory before org loads.
(setq org-directory "~/org/")

;; Configure org-mode for easier source block editing.
(use-package! org-tempo
  :after org
  :config
  (setq org-src-window-setup 'split-window-below
        org-src-fontify-natively t
        org-src-tab-acts-natively t)
  (add-to-list 'org-structure-template-alist '("sh" . "src shell"))
  (add-to-list 'org-structure-template-alist '("py" . "src python"))
  (add-to-list 'org-structure-template-alist '("el" . "src emacs-lisp")))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Python Development Environment Configuration
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;* Project Environment Management with direnv
;; Integrates `direnv` to automatically load project-specific environments.
(use-package! envrc
  :ensure t
  :config
  (envrc-global-mode +1))


;;* General Python Settings
;; Configures the built-in `python.el` package.
(use-package! python
  :config
  (setq python-indent-guess-indent-offset nil)
  (setq python-indent-offset 4))


;;* Language Server (LSP) with Pyright
;; Assumes `(:lang python +lsp +pyright)` is in init.el.
(use-package! lsp-pyright
  :ensure nil ; Managed by Doom
  :hook (python-ts-mode . (lambda ()
                            (require 'lsp-pyright)
                            (lsp)))
  :init
  (setq lsp-pyright-auto-import-completions t))


;;* Debugging with DAP and debugpy
;; Assumes `(:tools debugger +lsp)` is in init.el.
(use-package! dap-python
  :after (dap-mode)
  :config
  (setq dap-python-debugger 'debugpy)
  (dap-register-debug-template
   "Python :: Run Current File"
   (list :type "python"
         :request "launch"
         :name "Python :: Run Current File"
         :program "${file}"
         :console "integratedTerminal")))


;;* On-the-Fly Checking with Flymake
;; Assumes `(:checkers (syntax +flymake +icons))` is in your init.el.
(use-package! flymake
  :ensure nil
  :hook (python-ts-mode . flymake-mode)
  :config
  ;; Custom checker for the `bandit` security linter.
  (defun my/flymake-bandit-backend (report-fn &rest _args)
    "A flymake backend for the bandit security linter."
    (flymake-proc-run "bandit"
                      (list "-f" "json" (flymake-proc-source-file))
                      :reporter (flymake-proc-reporter-json-in-tmpfile
                                 report-fn
                                 (flymake-proc-json-parse-keys '("results")
                                   :line-key '("line_number")
                                   :text-key '("issue_text")
                                   :type-key '("issue_severity")
                                   :file-key '("filename")))))
  ;; Setup the checker chain for python-ts-mode.
  (defun my/python-ts-flymake-setup ()
    "Set up the flymake checker chain for Python."
    (setq-local flymake-diagnostic-functions
                (append flymake-diagnostic-functions
                        '(flymake-ruff
                          flymake-mypy
                          my/flymake-bandit-backend))))
  (add-hook 'python-ts-mode-hook #'my/python-ts-flymake-setup))


;;* Auto-formatting with Apheleia and Ruff
;; Assumes `(:editor format +onsave)` is in init.el.
(use-package! apheleia
  :ensure nil
  :config
  (setf (alist-get 'ruff apheleia-formatters)
        '("ruff" "format" "-"))
  (add-to-list 'apheleia-mode-alist '(python-ts-mode . ruff)))


;;* Jupyter Integration for Org Mode
;; Assumes `(:lang org +jupyter)` is in init.el.
;; Your Python environment must have `jupyter` and `ipykernel` installed.
(use-package! ob-jupyter
  :after org
  :config
  ;; Explicitly set the default kernel for python blocks to `python3`.
  (setq org-babel-jupyter-default-kernel "python3")
  ;; Override `python` blocks to send them to the Jupyter kernel.
  (with-eval-after-load 'ob-jupyter
    (org-babel-jupyter-override-src-block "python")))

;; Defer loading babel languages until after Org is loaded to prevent
;; blocking startup and ensure the Doom dashboard appears correctly.
(with-eval-after-load 'org
  (org-babel-do-load-languages
   'org-babel-load-languages
   '((python . t)
     (jupyter . t))))
