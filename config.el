(setq-default frame-title-format
              '(:eval (if (buffer-file-name)
                          (format "[%s] - %s"
                                  (file-name-nondirectory (buffer-file-name))
                                  (system-name))
                        (format "[%s] - %s"
                                (buffer-name)
                                (system-name)))))

(setq doom-font (font-spec :family "JetBrainsMono Nerd Font" :size 13.5 :weight 'medium)
      doom-variable-pitch-font (font-spec :family "JetBrainsMono Nerd Font" :size 13.5)
      doom-big-font (font-spec :family "JetBrainsMono Nerd Font" :size 26.0)
      doom-symbol-font (font-spec :family "JetBrainsMono Nerd Font" :size 13.5))

(use-package! doom-themes
  :custom
  (doom-themes-enable-bold t)
  (doom-themes-enable-italic t)
  (doom-themes-treemacs-theme "doom-tokyo-night")
  :config
  (load-theme 'doom-tokyo-night t)
  (doom-themes-visual-bell-config)
  (doom-themes-treemacs-config)
  (custom-set-faces
   '(bold ((t (:foreground "#7aa2f7" :weight bold))))
   '(italic ((t (:foreground "#bb9af7" :slant italic))))))

(after! vertico
        (setq vertico-count 10))

(after! corfu
        (setq corfu-auto-resize nil
              corfu-auto-delay 0.1))

;;; Basic Settings
(setq display-line-numbers-type t)
(setq org-directory "~/org/")

;;; Python Configuration
;; Remap python-mode to python-ts-mode
(add-to-list 'major-mode-remap-alist '(python-mode . python-ts-mode))

(use-package! org-tempo
  :after org
  :config
  (add-to-list 'org-structure-template-alist '("sh" . "src shell"))
  (add-to-list 'org-structure-template-alist '("py" . "src python"))
  (add-to-list 'org-structure-template-alist '("el" . "src emacs-lisp")))
