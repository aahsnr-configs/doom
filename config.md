```el
;;; -*- lexical-binding: t; -*-

;; Set user info
(setq user-full-name "Ahsanur Rahman"
      user-mail-address "ahsanur041@proton.me")


;;;; ------------------------------------------------------------------
;;;; CORE EMACS CONFIGURATION
;;;; ------------------------------------------------------------------

;; No beeping or blinking on errors.
(setq ring-bell-function 'ignore
      visible-bell nil)

;; Disable the obsolete practice of end-of-line spacing.
(setq sentence-end-double-space nil)

;; A line should end with a newline.
(setq require-final-newline t)

;; Improve scrolling behavior
(setq scroll-error-top-bottom t
      scroll-conservatively 120
      scroll-margin 0)

;; Prefer vertical splits over horizontal ones.
(setq split-width-threshold 170
      split-height-threshold nil)

;; Delete files by moving them to the system trash.
(setq delete-by-moving-to-trash t)

;; Disable cursor blink for a more responsive feel.
(blink-cursor-mode -1)

;;; --- Performance Tuning ---
(use-package! gcmh
  :config
  (gcmh-mode 1))

(use-package! so-long
  :hook (doom-startup . so-long-mode))


;;; --- PGTK/Wayland Mitigations ---
(setq-default bidi-display-reordering nil)
(when (fboundp 'pgtk-use-im-context)
  (add-hook 'after-make-frame-functions
            (lambda (frame)
              (with-selected-frame frame
                (pgtk-use-im-context nil)))))

;;;; ------------------------------------------------------------------
;;;; UI & THEMING
;;;; ------------------------------------------------------------------

;;; --- Theme ---
(setq doom-theme 'doom-tokyo-night)
(after! doom-themes
  (setq doom-themes-enable-bold t
        doom-themes-enable-italic t)
  (doom-themes-visual-bell-config)
  (doom-themes-treemacs-config)
  (doom-themes-org-config)
  ;; Set distinct colors for bold and italic from vanilla config
  (custom-set-faces
   '(bold ((t (:foreground "#7aa2f7" :weight bold))))
   '(italic ((t (:foreground "#bb9af7" :slant italic))))))


;;; --- Fonts ---
(setq doom-font (font-spec :family "JetBrainsMono Nerd Font" :size 14.0 :weight 'medium)
      doom-variable-pitch-font (font-spec :family "JetBrainsMono Nerd Font" :size 14.0)
      doom-big-font (font-spec :family "JetBrainsMono Nerd Font" :size 24.0))

(after! doom-themes
  (set-face-attribute 'font-lock-comment-face nil :slant 'italic)
  (set-face-attribute 'font-lock-keyword-face nil :slant 'italic))

(setq-default line-spacing 0.1)


;;; --- Which Key ---
(after! which-key
  (setq which-key-idle-delay 0.1
        which-key-separator " → "
        which-key-popup-type 'minibuffer))


;;; --- Info Colors ---
(use-package! info-colors
  :after info
  :hook (Info-selection . info-colors-fontify-node))


;;;; ------------------------------------------------------------------
;;;; EVIL (VIM) CONFIGURATION
;;;; ------------------------------------------------------------------
(setq evil-split-window-below t
      evil-vsplit-window-right t
      evil-want-fine-undo t
      evil-v$-excludes-newline t
      evil-move-beyond-eol t
      evil-search-wrap nil
      evil-want-Y-yank-to-eol t)

(after! evil-escape
  (setq evil-escape-key-sequence "jk"
        evil-escape-delay 0.2
        evil-escape-excluded-modes '(dired-mode)))

;; Navigate by visual lines
(map! :map evil-normal-state-map
      "j" #'evil-next-visual-line
      "k" #'evil-previous-visual-line)

(use-package! evil-lion
  :hook (prog-mode . evil-lion-mode))

(use-package! evil-multiedit
  :config (evil-multiedit-default-keybinds))

(use-package! evil-goggles
  :hook (evil-mode . evil-goggles-mode)
  :custom (evil-goggles-duration 0.1))

(use-package! evil-numbers :after evil)
(use-package! evil-args :after evil)
(use-package! evil-anzu :after evil)
(use-package! evil-exchange :after evil :config (evil-exchange-install))
(use-package! evil-indent-plus :after evil :config (evil-indent-plus-default-bindings))
(use-package! evil-visualstar :hook (evil-mode . global-evil-visualstar-mode))
(use-package! evil-matchit :hook (evil-mode . global-evil-matchit-mode))
(use-package! evil-snipe :after evil :config (evil-snipe-mode 1) (evil-snipe-override-mode 1))

(map! :map (normal visual) :leader :nv "gc" #'evilnc-comment-or-uncomment-lines)

;;;; ------------------------------------------------------------------
;;;; EDITOR BEHAVIOUR
;;;; ------------------------------------------------------------------

;; Highlight current line and use visual-line soft wrapping
(global-hl-line-mode)
(add-hook 'doom-startup-hook #'global-visual-line-mode)


;;; --- Smartparens ---
(after! smartparens
  (sp-pair "<" nil :actions :rem)
  (setq sp-show-pair-delay 0.1
        sp-show-pair-from-inside t))


;;; --- Rainbow Delimiters ---
(use-package! rainbow-delimiters
  :hook ((prog-mode . rainbow-delimiters-mode)
         (org-src-mode . rainbow-delimiters-mode))
  :custom-face
  (rainbow-delimiters-depth-1-face ((t (:foreground "#7aa2f7"))))  ; Blue
  (rainbow-delimiters-depth-2-face ((t (:foreground "#bb9af7"))))  ; Magenta
  (rainbow-delimiters-depth-3-face ((t (:foreground "#e0af68"))))  ; Yellow
  (rainbow-delimiters-depth-4-face ((t (:foreground "#73daca"))))  ; Cyan
  (rainbow-delimiters-depth-5-face ((t (:foreground "#f7768e"))))  ; Red
  (rainbow-delimiters-depth-6-face ((t (:foreground "#9ece6a"))))  ; Green
  (rainbow-delimiters-depth-7-face ((t (:foreground "#ff9e64"))))  ; Orange
  (rainbow-delimiters-depth-8-face ((t (:foreground "#c0caf5"))))  ; Foreground
  (rainbow-delimiters-depth-9-face ((t (:foreground "#a9b1d6"))))) ; Sub-Foreground


;;; --- Automatic Buffer Cleanup ---
(use-package! buffer-terminator
  :custom
  (buffer-terminator-verbose nil)
  (buffer-terminator-inactivity-timeout (* 30 60)) ; 30 minutes
  (buffer-terminator-interval (* 10 60)) ; 10 minutes
  :config
  (buffer-terminator-mode 1))


;;; --- Shackle for Popup Window Management ---
(use-package! shackle
  :hook (doom-startup . shackle-mode)
  :config
  (setq shackle-rules
   '(("\\`\\*Help" :align bottom :size 0.3)
     ("^\\*.*compilation.*\\*$" :align bottom :size 0.3)
     ("^\\*grep.*\\*$" :align bottom :size 0.3)
     ("\\`\\*Embark Collect" :align bottom :size 0.25)
     ("\\`\\*dap-repl" :align right :size 0.4)
     ("\\`\\*dap-locals" :align right :size 0.4)
     ("\\`\\*dap-breakpoints" :align right :size 0.4)
     ("\\`\\*dap-sessions" :align right :size 0.4))
   shackle-inhibit-window-quit-on-same-buffer t))


;;; --- Helpful ---
(use-package! helpful
  :bind
  ([remap describe-command] . helpful-command)
  ([remap describe-function] . helpful-callable)
  ([remap describe-key] . helpful-key)
  ([remap describe-symbol] . helpful-symbol)
  ([remap describe-variable] . helpful-variable)
  :custom
  (helpful-max-buffers 7))


;;; --- Writable Grep ---
(use-package! wgrep
  :config
  (setq wgrep-auto-save-buffer t))

;;; --- Spell Checking (Jinx) ---
(use-package! jinx
  :hook (doom-startup . jinx-mode)
  :custom
  (jinx-languages "en_US")
  (jinx-delay 0.3)
  (jinx-disabled-modes
   '(prog-mode conf-mode emacs-lisp-mode dired-mode ibuffer-mode treemacs-mode
     magit-status-mode magit-log-mode magit-diff-mode magit-branch-mode
     org-agenda-mode org-src-mode dashboard-mode which-key-mode help-mode
     Info-mode embark-collect-mode vterm-mode pdf-view-mode)))


;;;; ------------------------------------------------------------------
;;;; COMPLETION FRAMEWORK
;;;; ------------------------------------------------------------------

;;; --- Vertico ---
(after! vertico
  (setq vertico-resize nil
        vertico-count 10
        vertico-cycle t))

;;; --- Marginalia ---
(use-package! nerd-icons-completion
  :hook (marginalia-mode . nerd-icons-completion-marginalia-setup)
  :config (nerd-icons-completion-mode))

;;; --- Consult ---
(after! consult
  (setq register-preview-delay 0.3
        consult-preview-key 'any
        consult-narrow-key "<"
        consult-find-args "fd --hidden --strip-cwd --type f --color=never --follow --exclude .git"
        consult-ripgrep-args "rg --null --line-buffered --color=never --smart-case --no-heading --line-number --hidden --glob '!.git/'")

  ;; Powerful buffer switcher from vanilla config
  (setq consult-buffer-sources
   '(consult--source-recent-file
     consult--source-project-recent-file
     consult--source-buffer
     consult--source-bookmark))

  (consult-customize
   consult-theme :preview-key '(:debounce 0.05 any)
   consult-ripgrep consult-git-grep consult-grep
   consult-bookmark consult-recent-file consult-xref
   consult--source-bookmark consult--source-file-register
   consult--source-recent-file consult--source-project-recent-file
   :preview-key '(:debounce 0.1 any)))

;;; --- Embark ---
(after! embark
  (define-key embark-collect-mode-map (kbd "e") #'embark-export)
  (add-to-list 'display-buffer-alist
               '("\\`\\*Embark Collect \\(Live\\|Completions\\)\\*"
                 nil
                 (window-parameters (mode-line-format . none))))
  (setq prefix-help-command #'embark-prefix-help-command))

(use-package! embark-consult
  :after (embark consult)
  :hook (embark-collect-mode . consult-preview-at-point-mode))


;;; --- Corfu ---
(after! corfu
  (setq corfu-cycle t
        corfu-auto t
        corfu-auto-resize nil
        corfu-auto-delay 0.13
        corfu-preselect 'prompt
        corfu-quit-at-boundary 'separator
        corfu-quit-no-match 'separator
        corfu-on-exact-match nil)
  (corfu-history-mode)
  (corfu-popupinfo-mode))

(use-package! nerd-icons-corfu
  :after (corfu nerd-icons)
  :config (add-to-list 'corfu-margin-formatters #'nerd-icons-corfu-formatter))



;;;; ------------------------------------------------------------------
;;;; ORG MODE & ROAM
;;;; ------------------------------------------------------------------

;;; --- Directory Structure ---
(defvar my/org-directory (expand-file-name "~/org/") "Base directory for all org files.")
(defvar my/org-roam-directory (expand-file-name "roam/" my/org-directory) "Directory for org-roam files.")
(defvar my/org-noter-directory (expand-file-name "noter/" my/org-directory) "Directory for org-noter files.")
(defvar my/org-archive-directory (expand-file-name "archive/" my/org-directory) "Directory for archived org files.")
(dolist (dir (list my/org-directory my/org-roam-directory my/org-noter-directory my/org-archive-directory))
  (unless (file-directory-p dir)
    (make-directory dir t)))


;;; --- Org Core Configuration ---
(after! org
  (setq org-directory my/org-directory
        org-ellipsis " ⤵"
        org-log-done 'time
        org-log-into-drawer t
        org-startup-with-inline-images t
        org-image-actual-width 600
        org-auto-align-tags nil
        org-confirm-babel-evaluate nil
        org-src-fontify-natively t
        org-src-tab-acts-natively t
        org-hide-emphasis-markers t)

  (add-hook! 'org-mode-hook
             (defun +my/org-mode-setup ()
               (org-indent-mode)
               (visual-line-mode 1)
               (+org-pretty-mode)
               ;; Evil friendly TAB in org-mode
               (evil-define-key 'normal org-mode-map (kbd "TAB") 'org-cycle)))

  ;; Use Tectonic for LaTeX exports
  (setq org-latex-compiler "tectonic")
  (setq org-latex-pdf-process '("tectonic -X compile %f -o %o"))

  (setq org-todo-keywords
        '((sequence "📥 TODO(t)" "⚡ NEXT(n)" "⚙️ PROG(p)" "⏳ WAIT(w@/!)" "|" "✅ DONE(d!)" "❌ CANCEL(c@)")
          (sequence "📝 PLAN(P)" "🚀 ACTIVE(A)" "⏸️ PAUSED(x)" "|" "🏆 ACHIEVED(a)" "🗑️ DROPPED(D)")))
  (setq org-todo-keyword-faces
        '(("📥 TODO"      . (:foreground "#f7768e" :weight bold))
          ("⚡ NEXT"      . (:foreground "#ff9e64" :weight bold))
          ("⚙️ PROG"      . (:foreground "#7aa2f7" :weight bold))
          ("⏳ WAIT"      . (:foreground "#e0af68" :weight bold))
          ("✅ DONE"      . (:foreground "#9ece6a" :weight bold))
          ("❌ CANCEL"    . (:foreground "#565f89" :weight bold))
          ("📝 PLAN"      . (:foreground "#73daca" :weight bold))
          ("🚀 ACTIVE"    . (:foreground "#bb9af7" :weight bold))
          ("⏸️ PAUSED"    . (:foreground "#c0caf5" :weight bold))
          ("🏆 ACHIEVED"  . (:foreground "#9ece6a" :weight bold))
          ("🗑️ DROPPED"   . (:foreground "#565f89" :weight bold))))

  (use-package! org-tempo
    :config
    (add-to-list 'org-structure-template-alist '("sh" . "src shell"))
    (add-to-list 'org-structure-template-alist '("el" . "src emacs-lisp"))
    (add-to-list 'org-structure-template-alist '("py" . "src python"))))


;;; --- Org Visuals ---
(use-package! org-modern
  :hook (org-mode . org-modern-mode)
  :config
  (setq org-modern-hide-stars "· "
        org-modern-star '("◉" "○" "◈" "◇" "◆" "▷")
        org-modern-list '((43 . "➤") (45 . "–") (42 . "•"))
        org-modern-table-vertical 1
        org-modern-table-horizontal 0.1
        org-modern-block-name '(("src" "»" "«") ("example" "»" "«") ("quote" "❝" "❞"))
        org-modern-tag-faces `((:foreground ,(face-attribute 'default :foreground) :weight bold :box (:line-width (1 . -1) :color "#3b4261")))
        org-modern-checkbox '((todo . "☐") (done . "☑") (cancel . "☒") (priority . "⚑") (on . "◉") (off . "○"))))

(use-package! org-appear
  :hook (org-mode . org-appear-mode)
  :config
  (setq org-appear-autoemphasis t
        org-appear-autolinks t
        org-appear-autosubmarkers t))


;;; --- Org Capture ---
;; Function to find projects for dynamic capture template
(defun ar/find-org-projects ()
  "Return a list of all Org files with a \"project\" tag for capture."
  (let* ((builder (consult--grep-builder
                   (list consult-ripgrep-args
                         "--files-with-matches"
                         "--glob=*.org"
                         "^#\\+filetags:.*:project:.*"
                         (expand-file-name my/org-directory)))))
    (mapcar (lambda (file)
              (list (file-name-nondirectory file) file))
            (consult--grep-sync builder))))

(after! org-capture
  (setq org-capture-templates
   '(("t" "📥 Task" entry (file+headline "~/org/inbox.org" "Tasks")
      "* 📥 TODO %?\n  :PROPERTIES:\n  :CREATED: %U\n  :END:\n")
     ("n" "📝 Note" entry (file+headline "~/org/inbox.org" "Notes")
      "* %? :note:\n  :PROPERTIES:\n  :CREATED: %U\n  :END:\n")
     ("j" "📔 Journal" entry (file+olp+datetree "~/org/journal.org")
      "* %U %?\n")
     ("p" "📝 Project" entry (file+headline "~/org/projects.org" "Projects")
      "* 📝 PLAN %? :project:\n  :PROPERTIES:\n  :CREATED: %U\n  :END:\n** Goals\n** Tasks\n*** 📥 TODO Define project scope\n")
     ("P" "📌 Project Task" entry
      (file (lambda ()
              (let* ((project-list (ar/find-org-projects))
                     (project-name (completing-read "Select Project: " project-list)))
                (cdr (assoc project-name project-list)))))
      "* 📥 TODO %?\n  :PROPERTIES:\n  :CREATED: %U\n  :END:\n"
      :prepend t
      :headline "Tasks")
     ("b" "📚 Book" entry (file+headline "~/org/reading.org" "Reading List")
      "* %? :book:read:\n  :PROPERTIES:\n  :CREATED: %U\n  :END:\n")
     ("g" "🎯 Goal" entry (file+headline "~/org/goals.org" "Goals")
      "* 🎯 GOAL %? :goal:\n  :PROPERTIES:\n  :CREATED: %U\n  :END:\n"))))


;;; --- Org Agenda ---
(after! org-agenda
  (setq org-agenda-files (list org-directory)
        org-agenda-skip-scheduled-if-done t
        org-agenda-skip-deadline-if-done t
        org-agenda-include-deadlines t
        org-agenda-block-separator 'hr
        org-agenda-compact-blocks t
        org-agenda-start-with-log-mode t)
  (org-super-agenda-mode)

  (setq org-agenda-custom-commands
        '(("d" "📅 Dashboard"
           ((agenda "" ((org-deadline-warning-days 7)
                        (org-agenda-overriding-header "📅 Agenda")))
            (todo "⚡ NEXT" ((org-agenda-overriding-header "⚡ Next Tasks")))
            (tags-todo "project/🚀 ACTIVE" ((org-agenda-overriding-header "🚀 Active Projects")))
            (tags-todo "+PRIORITY=\"A\"" ((org-agenda-overriding-header "🔥 High Priority")))
            (todo "⏳ WAIT" ((org-agenda-overriding-header "⏳ Waiting On")))
            (tags-todo "+habit" ((org-agenda-overriding-header "🔄 Habits")))
            (stuck "" ((org-agenda-overriding-header "🚫 Stuck Projects")))))
          ("p" "📋 Projects Overview"
           ((tags "project" ((org-agenda-overriding-header "📋 All Projects")))))
          ("g" "🎯 Goals Review"
           ((tags-todo "goal" ((org-agenda-overriding-header "🎯 Goals")))))))

  (setq org-super-agenda-groups
        '((:name "🔥 Overdue" :deadline past)
          (:name "📅 Today" :time-grid t :scheduled today)
          (:name "⚡ Next" :todo "⚡ NEXT")
          (:name "🔥 Important" :priority "A")
          (:name "📋 Projects" :tag "project")
          (:name "⏳ Waiting" :todo "⏳ WAIT")
          (:discard (:anything t)))))


;;; --- Org Roam ---
(after! org-roam
  (setq org-roam-directory my/org-roam-directory)
  (setq org-roam-db-location (expand-file-name "org-roam.db" doom-local-dir))
  (setq org-roam-node-display-template
        (concat "${title:*} " (propertize "${tags:20}" 'face 'org-tag)))
  (setq org-roam-capture-templates
        '(("d" "default" plain "* %?"
           :target (file+head "${slug}.org" "#+title: ${title}\n\n")
           :unnarrowed t)
          ("p" "project" plain "* Goal\n\n%?\n\n* Tasks"
           :target (file+head "projects/${slug}.org" "#+title: Project: ${title}\n#+filetags: project\n")
           :unnarrowed t)
          ("l" "literature note" plain "* Source\n\n* Summary\n\n%?"
           :target (file+head "literature/${slug}.org" "#+title: ${title}\n#+filetags: literature\n")
           :unnarrowed t)))

  (add-to-list 'display-buffer-alist
               '("\\*org-roam\\*"
                 (display-buffer-in-direction)
                 (direction . right)
                 (window-width . 0.33)
                 (window-height . fit-window-to-buffer))))

(use-package! org-roam-ui
  :after org-roam
  :config
  (setq org-roam-ui-sync-theme t
        org-roam-ui-follow t
        org-roam-ui-update-on-save t
        org-roam-ui-open-on-start nil))

(use-package! consult-org-roam
  :after (consult org-roam)
  :config (consult-org-roam-mode 1))


;;;; ------------------------------------------------------------------
;;;; WORKFLOW MANAGEMENT
;;;; ------------------------------------------------------------------

;;; --- Buffer Management (Ibuffer) ---
(after! ibuffer
  (setq ibuffer-never-show-regexps
        '("\\` " "\\*dashboard\\*" "\\*scratch\\*" "\\*Messages\\*" "\\*Help\\*" "\\*Embark Collect"))
  (setq ibuffer-formats
        '((mark modified read-only " "
                (icon 4 4 :left :elide)
                (name 35 35 :left :elide) " "
                (size-h 9 9 :right :elide) " "
                (mode 16 16 :left :elide) " "
                filename-and-process))))

(defun ar/ibuffer-setup-hook ()
  "Set up ibuffer with project grouping, icons, sorting, and evil keys."
  (ar/ibuffer-set-project-groups)
  (ibuffer-do-sort-by-last-access-time)
  (ibuffer-update nil t))
(add-hook 'ibuffer-hook #'ar/ibuffer-setup-hook)

(defun ar/ibuffer-set-project-groups ()
  "Create and set ibuffer filter groups based on known projects."
  (let ((groups '()))
    (dolist (proj (projectile-project-list))
      (let* ((proj-name (projectile-project-name proj))
             (proj-root (projectile-project-root proj)))
        (push `(,proj-name (:eval (and (buffer-file-name)
                                      (string-prefix-p proj-root (buffer-file-name)))))
              groups)))
    (push '("Miscellaneous" (:predicate (lambda (buf)
                                          (and (buffer-file-name buf)
                                               (not (projectile-project-p (buffer-file-name buf)))))))
          groups)
    (setq ibuffer-filter-groups (nreverse groups))))

(use-package! nerd-icons-ibuffer
  :hook (ibuffer-mode . nerd-icons-ibuffer-mode))

;; Evil keybindings for ibuffer
(evil-define-key 'normal ibuffer-mode-map
  (kbd "j") 'ibuffer-next-line
  (kbd "k") 'ibuffer-previous-line
  (kbd "d") 'ibuffer-mark-for-delete
  (kbd "x") 'ibuffer-do-delete
  (kbd "s") 'ibuffer-do-save
  (kbd "g") 'revert-buffer
  (kbd "q") 'quit-window)


;;; --- Dired ---
(use-package! fd-dired :config (setq fd-dired-use-gnu-find-syntax t))
(use-package! dired-open :config (setq dired-open-extensions '(("png" . "imv") ("mp4" . "mpv"))))

(after! dired
  (setq dired-listing-switches "-agho --group-directories-first"
        dired-omit-files "^\\.[^.]\\|^#\\|^\\.$\\|^\\.\\.$\\|\\.pyc$\\|\\.o$")
  (evil-define-key 'normal dired-mode-map
    (kbd "h") 'dired-up-directory
    (kbd "l") 'dired-find-file-other-window
    (kbd "^") 'dired-goto-root-directory
    (kbd "~") 'dired-home
    (kbd "C-n") 'dired-create-file
    (kbd "C-d") 'dired-create-directory
    (kbd "R") 'dired-do-rename
    (kbd "X") 'dired-open-file))

(use-package! nerd-icons-dired
  :hook (dired-mode . nerd-icons-dired-mode))

(use-package! dired-ranger
  :config
  (define-key dired-mode-map (kbd "y") 'dired-ranger-copy)
  (define-key dired-mode-map (kbd "p") 'dired-ranger-paste)
  (define-key dired-mode-map (kbd "x") 'dired-ranger-move))


;;;; ------------------------------------------------------------------
;;;; VERSION CONTROL (GIT)
;;;; ------------------------------------------------------------------

;;; --- Magit ---
(after! magit
  (setq magit-display-buffer-function #'magit-display-buffer-fullframe-status-v1)
  (defun ar/magit-quit-and-restore-windows ()
    "Kill Magit buffer and restore previous window configuration."
    (interactive)
    (kill-buffer (current-buffer))
    (when (get-register :magit-fullscreen)
      (jump-to-register :magit-fullscreen)))
  (define-key magit-status-mode-map (kbd "q") #'ar/magit-quit-and-restore-windows))

(use-package! magit-todos
  :hook (magit-mode . magit-todos-mode))

(use-package! git-timemachine)


;;; --- Git Gutter ---
(after! vc-gutter
  (setq git-gutter:update-on-save t
        git-gutter:update-method "idle")
  (map! :leader :desc "Next hunk" "]g" #'git-gutter:next-hunk
              :desc "Previous hunk" "[g" #'git-gutter:previous-hunk))


;;;; ------------------------------------------------------------------
;;;; PDF & NOTER
;;;; ------------------------------------------------------------------
(after! pdf-tools
  (setq pdf-view-midnight-colors (cons (doom-color 'bg) (doom-color 'fg))
        pdf-view-continuous t)
  (set-face-attribute 'pdf-view-highlight-face nil :background (doom-color 'cyan))
  (add-hook! 'pdf-view-mode-hook
    (defun +my/pdf-view-mode-setup ()
      (pdf-view-midnight-mode 1)
      (pdf-view-fit-width-to-window)))

  (map! :map pdf-view-mode-map
        :n "J"       #'pdf-view-next-page
        :n "K"       #'pdf-view-previous-page))


;;; --- Org Noter ---
(after! org-noter
  (setq org-noter-notes-search-path (list my/org-noter-directory)
        org-noter-notes-file-name "%s.org"
        org-noter-insert-note-no-questions t
        org-noter-always-focus-on-notes-buffer t
        org-noter-note-heading-template "* %s\n:PROPERTIES:\n:NOTER_PAGE: %p\n:END:\n\n"))

(defun ar/org-noter-find-or-create-notes ()
  "Find the notes for the current PDF or create a new notes file."
  (interactive)
  (let ((pdf-path (buffer-file-name)))
    (unless pdf-path (error "Current buffer is not visiting a file"))
    (let* ((pdf-name (file-name-nondirectory pdf-path))
           (notes-file (expand-file-name (format "%s.org" (file-name-sans-extension pdf-name)) my/org-noter-directory)))
      (if (file-exists-p notes-file)
          (find-file notes-file)
        (progn
          (find-file notes-file)
          (insert (format "#+title: Notes on %s\n\n" pdf-name))))
      (delete-other-windows)
      (split-window-right)
      (windmove-right)
      (find-file pdf-path))))

;;;; ------------------------------------------------------------------
;;;; SNIPPETS (YASNIPPET)
;;;; ------------------------------------------------------------------
(use-package! yasnippet-capf
  :config
  (add-to-list 'completion-at-point-functions #'yasnippet-capf))


;;;; ------------------------------------------------------------------
;;;; UTILITY FUNCTIONS & KEYBINDINGS
;;;; ------------------------------------------------------------------


;;; --- Global Keybindings ---
(map! :leader
      :desc "Reload config" "q r" #'+my/reload-config
      ;; Org
      "o" '(:ignore t :wk "org")
      "oa" '(org-agenda :wk "agenda")
      "oc" '(org-capture :wk "capture")
      ;; Org Roam
      "or" '(:ignore t :wk "roam")
      "orf" '(org-roam-node-find :wk "find node")
      "ori" '(org-roam-node-insert :wk "insert node")
      "orc" '(org-roam-capture :wk "roam capture")
      "org" '(org-roam-ui-open :wk "show graph")
      ;; Org Noter
      "on" '(:ignore t :wk "noter")
      "onn" '(ar/org-noter-find-or-create-notes :wk "Open/Create PDF Notes")
      "oni" '(org-noter-insert-note :wk "Insert Note")
      ;; Buffers
      "b"   '(:ignore t :wk "buffers")
      "bi" '(ibuffer :wk "ibuffer (by project)")
      ;; Snippets
      "s" '(:ignore t :wk "snippets")
      "si" '(consult-yasnippet :wk "insert snippet (consult)")
      "sn" '(yas-new-snippet :wk "new snippet")
      ;; Compile/Cite
      "c" '(:ignore t :wk "compile/cite")
      "cc" '(TeX-command-master :wk "Compile Document")
      "cv" '(TeX-view :wk "View Output")
      "cb" '(citar-insert-citation :wk "Insert Citation"))

```
