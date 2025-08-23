# config.el

```el
;;; -*- lexical-binding: t; -*-

(setq user-full-name "Ahsanur Rahman"
      user-mail-address "ahsanur041@proton.me")

(defalias 'yes-or-no-p 'y-or-n-p)

(use-package doom-themes
  :custom
  (doom-themes-enable-bold t)
  (doom-themes-enable-italic t)
  (doom-themes-treemacs-theme "doom-tokyo-night")
  :config
  (load-theme 'doom-tokyo-night t)

  (doom-themes-visual-bell-config)
  (doom-themes-treemacs-config)
  (doom-themes-org-config))

;; (setq doom-theme 'catppuccin)

(setq doom-font (font-spec :family "JetBrainsMono Nerd Font" :size 14.5 :weight 'medium)
      doom-variable-pitch-font (font-spec :family "JetBrainsMono Nerd Font" :size 14.5)
      doom-big-font (font-spec :family "JetBrainsMono Nerd Font" :size 26.0))

(setq doom-unicode-font (font-spec :family "JetBrainsMono Nerd Font" :size 14.5))

(add-hook! 'doom-after-init-hook
  (defun +my/setup-font-faces ()
    (set-face-attribute 'font-lock-comment-face nil :slant 'italic)
    (set-face-attribute 'font-lock-keyword-face nil :slant 'italic)))

(setq-default line-spacing 0.00)

(add-hook! '(org-mode-hook term-mode-hook shell-mode-hook eshell-mode-hook)
           #'(lambda () (display-line-numbers-mode -1)))

(setq +doom-dashboard-banner-padding '(0 . 2))
(setq +doom-dashboard-banner-file "~/.config/doom/banner.png")

(setq which-key-allow-multiple-replacements t)
(after! which-key
  (pushnew!
   which-key-replacement-alist
   '(("" . "\\`+?evil[-:]?\\(?:a-\\)?\\(.*\\)") . (nil . " \\1"))
   '(("\\`g s" . "\\`evilem--?motion-\\(.*\\)") . (nil . " \\1"))))

(after! doom-modeline
  (setq doom-modeline-buffer-file-name-style 'truncate-upto-project
        doom-modeline-display-buffer-encoding nil
        doom-modeline-display-minor-modes nil))

(setq evil-split-window-below t
      evil-vsplit-window-right t
      evil-want-fine-undo t)

;; Custom cursor colors from vanilla config
(setq evil-normal-state-cursor '(box "#fe8019")
      evil-insert-state-cursor '(bar "#fb4934")
      evil-visual-state-cursor '(hollow "#fe8019"))

(after! evil-escape
  (setq evil-escape-key-sequence "jk"
        evil-escape-delay 0.2
        evil-escape-excluded-modes '(dired-mode)))

(after! evil-goggles
  (setq evil-goggles-duration 0.1))

(map! :map evil-normal-state-map
      "j" #'evil-next-visual-line
      "k" #'evil-previous-visual-line)

(after! vertico
  (setq vertico-count 10
        vertico-cycle t))

(after! consult
  ;; Use fd and rg for faster searching, from vanilla config
  (setq consult-find-args "fd --hidden --strip-cwd --type f --color=never"
        consult-ripgrep-args "rg --null --line-buffered --color=never --smart-case --no-heading --line-number --hidden --glob '!.git/'"))

(use-package! consult-yasnippet
  :after (consult yasnippet)
  :config
  (setq consult-yasnippet-category-icon-alist
        '((t . "»")
          ("Emacs Lisp" . "λ")
          ("Text" . "¶")
          ("Org" . "★")
          ("Python" . "🐍"))))

(map! :leader
      :desc "Search snippets" "s y" #'consult-yasnippet)

;; Kill buffer on emacs exit
(setq vterm-kill-buffer-on-exit t)

;; Prevent kill on closing emacs frame
(defun +my/vterm-prevent-kill-on-frame-close-query ()
  "Prevent killing vterm buffers when closing a frame in daemon mode."
  (if (and (daemonp) (eq major-mode 'vterm-mode))
      ;; If in daemon mode and it's a vterm buffer, prevent the kill query
      ;; and keep the buffer alive in the daemon.
      nil
    ;; Otherwise, allow default kill query behavior.
    t))

;; Add the function to the `kill-buffer-query-functions` hook.
(add-hook 'kill-buffer-query-functions #'+my/vterm-prevent-kill-on-frame-close-query)

;; Force kill vterm buffer
(defun +my/vterm-force-kill-current-buffer ()
  (interactive)
  (when (eq major-mode 'vterm-mode)
    (kill-buffer (current-buffer) t)
    (message "Vterm buffer killed forcefully.")))

(map! :leader
      :desc "Toggle vterm locally"  "v t" #'+vterm/toggle
      :desc "Open vterm buffer locally" "v T" #'+vterm/here
      :desc "Force kill current vterm buffer" "o k" #'+my/vterm-force-kill-current-buffer)

(after! marginalia
  (setq marginalia-censor-variables nil)

  (defadvice! +marginalia--anotate-local-file-colorful (cand)
    "Just a more colourful version of `marginalia--anotate-local-file'."
    :override #'marginalia--annotate-local-file
    (when-let (attrs (file-attributes (substitute-in-file-name
                                       (marginalia--full-candidate cand))
                                      'integer))
      (marginalia--fields
       ((marginalia--file-owner attrs)
        :width 12 :face 'marginalia-file-owner)
       ((marginalia--file-modes attrs))
       ((+marginalia-file-size-colorful (file-attribute-size attrs))
        :width 7)
       ((+marginalia--time-colorful (file-attribute-modification-time attrs))
        :width 12))))

  (defun +marginalia--time-colorful (time)
    (let* ((seconds (float-time (time-subtract (current-time) time)))
           (color (doom-blend
                   (face-attribute 'marginalia-date :foreground nil t)
                   (face-attribute 'marginalia-documentation :foreground nil t)
                   (/ 1.0 (log (+ 3 (/ (+ 1 seconds) 345600.0)))))))
      ;; 1 - log(3 + 1/(days + 1)) % grey
      (propertize (marginalia--time time) 'face (list :foreground color))))

  (defun +marginalia-file-size-colorful (size)
    (let* ((size-index (/ (log10 (+ 1 size)) 7.0))
           (color (if (< size-index 10000000) ; 10m
                      (doom-blend 'orange 'green size-index)
                    (doom-blend 'red 'orange (- size-index 1)))))
      (propertize (file-size-human-readable size) 'face (list :foreground color)))))

(use-package! info-colors
  :after info
  :commands (info-colors-fontify-node)
  :hook (Info-selection . info-colors-fontify-node))

(use-package! jinx
  :defer t
  :hook ((text-mode . jinx-mode)
         (prog-mode . jinx-mode) ; Also useful for checking comments/strings in code
         (org-mode . jinx-mode)
         (markdown-mode . jinx-mode)
         (git-commit-mode . jinx-mode))
  :bind (("M-$" . jinx-correct)
         ("C-M-$" . jinx-languages))
  :init
  (defvar my-jinx-ignored-words
    '("DoomEmacs" "Elisp" "EmacsLisp" "use-package" "tecosaur"
      "jinx-mode" "prog-mode" "conf-mode" "WIP" "regexp" "Ahsanur"
      "Rahman" "toc" "LaTeX" "cleverparens" "parens"))
  :config
  (setq jinx-languages "en_US")
  (setq jinx-delay 0.3)

  (push `(t . (,(concat "\\<\\(" (mapconcat #'regexp-quote my-jinx-ignored-words "\\|") "\\)\\>")))
        jinx-exclude-regexps)
  (push '(org-mode
          ;; All headline levels
          org-level-1 org-level-2 org-level-3 org-level-4
          org-level-5 org-level-6 org-level-7 org-level-8
          ;; Other Org elements
          org-document-title
          org-block
          org-src-block
          org-meta-line
          org-table
          org-link) ; Ignore URLs in links
        jinx-exclude-faces)
  (after! vertico
    (when (boundp 'vertico-multiform-categories)
      (add-to-list 'vertico-multiform-categories '(jinx (vertico-grid-annotate . t))))))

(after! pdf-tools
  (setq pdf-view-midnight-colors (cons (doom-color 'bg) (doom-color 'fg)))
  (set-face-attribute 'pdf-view-highlight-face nil :background (doom-color 'cyan))
  (add-hook! 'pdf-view-mode-hook
    (defun +my/pdf-view-mode-setup ()
      (auto-revert-mode 1)
      (pdf-view-continuous-scroll-mode 1)
      (pdf-view-midnight-mode 1)
      (pdf-view-fit-width-to-window)))

  (map! :map pdf-view-mode-map
        ;; Page Navigation
        :n "J"       #'pdf-view-next-page
        :n "K"       #'pdf-view-previous-page
        :n "gg"      #'pdf-view-first-page
        :n "G"       #'pdf-view-last-page
        :n "C-f"     #'pdf-view-scroll-down-or-next-page
        :n "C-b"     #'pdf-view-scroll-up-or-previous-page
        ;; Horizontal Scrolling
        :n "h"       #'pdf-view-scroll-left
        :n "l"       #'pdf-view-scroll-right
        ;; Zooming
        :n "+"       #'pdf-view-scale-up
        :n "-"       #'pdf-view-scale-down
        :n "zi"      #'pdf-view-scale-up      ; mnemonic: zoom in
        :n "zo"      #'pdf-view-scale-down    ; mnemonic: zoom out
        ;; Fitting Commands
        :n "="       #'pdf-view-fit-page-to-window
        :n "zw"      #'pdf-view-fit-width-to-window ; mnemonic: zoom width
        :n "zh"      #'pdf-view-fit-height-to-window; mnemonic: zoom height
        ;; History Navigation (like a web browser)
        :n "C-o"     #'pdf-history-backward
        :n "C-i"     #'pdf-history-forward
        ;; Outline (Table of Contents)
        :n "o"       #'pdf-outline
        ;; SyncTeX (for LaTeX integration)
        :n "gs"      #'pdf-sync-forward-search)) ; mnemonic: go source

(after! dired
  ;; Omit files like in the vanilla config
  (setq dired-omit-files "^\\.[^.]\\|^#\\|^\\.$\\|^\\.\\.$\\|\\.pyc$\\|\\.o$")
  (setq dired-listing-switches "-agho --group-directories-first"))

(after! dirvish
  ;; Set quick access directories from vanilla config
  (setq dirvish-quick-access-entries
        '(("h" "~/" "Home")
          ("d" "~/Downloads/" "Downloads")
          ("D" "~/Documents/" "Documents")
          ("p" "~/Projects/" "Projects")
          ("/" "/" "Root")))
  (setq dirvish-attributes '(nerd-icons file-time file-size collapse subtree-state vc-state)))

(defun ar/org-font-setup ()
  ;; Set faces for heading levels
  (dolist (face '((org-level-1 . 1.2)
                  (org-level-2 . 1.1)
                  (org-level-3 . 1.05)
                  (org-level-4 . 1.0)
                  (org-level-5 . 1.1)
                  (org-level-6 . 1.1)
                  (org-level-7 . 1.1)
                  (org-level-8 . 1.1)))
    (set-face-attribute (car face) nil :font "JetBrainsMono Nerd Font" :weight 'bold :height (cdr face) :slant 'unspecified))

  ;; Ensure that anything that should be fixed-pitch in Org files appears that way
  (set-face-attribute 'org-tag nil :foreground nil :inherit '(shadow fixed-pitch) :weight 'bold)
  (set-face-attribute 'org-block nil :foreground nil :inherit 'fixed-pitch)
  (set-face-attribute 'org-code nil   :inherit '(shadow fixed-pitch))
  (set-face-attribute 'org-table nil   :inherit '(shadow fixed-pitch))
  (set-face-attribute 'org-verbatim nil :inherit '(shadow fixed-pitch))
  (set-face-attribute 'org-special-keyword nil :inherit '(font-lock-comment-face fixed-pitch))
  (set-face-attribute 'org-meta-line nil :inherit '(font-lock-comment-face fixed-pitch))
  (set-face-attribute 'org-checkbox nil :inherit 'fixed-pitch))

(defun ar/org-setup-hook ()
  "Modes to enable on org-mode start"
  (org-indent-mode)
  (visual-line-mode 1)
  (+org-pretty-mode)
  (ar/org-font-setup))

(after! org
  (setq org-directory "~/org"
        org-ellipsis " "
        org-startup-with-inline-images t
        org-image-actual-width 600
        org-archive-location "archive/Archive_%s::"
        org-auto-align-tags nil)

  (add-hook! org-mode #'ar/org-setup-hook))

(use-package! org-tempo
  :after org
  :config
  (setq org-src-window-setup 'split-window-below
        org-src-fontify-natively t
        org-src-tab-acts-natively t)
  (add-to-list 'org-structure-template-alist '("sh" . "src shell"))
  (add-to-list 'org-structure-template-alist '("py" . "src python"))
  (add-to-list 'org-structure-template-alist '("el" . "src emacs-lisp")))

(use-package! visual-fill-column
  :hook (org-mode . visual-fill-column-mode)
  :custom
  (visual-fill-column-width 100)
  (visual-fill-column-center-text t))

(setf (alist-get 'height +org-capture-frame-parameters) 15)

(after! org
  (setq org-todo-keywords
        '((sequence "☛ TODO(t)" "⚡ NEXT(n)" "🔄 PROG(p)" "⏳ WAIT(w@/!)" "|" "✅ DONE(d!)" "❌ CANCELLED(c@)")
          (sequence "🎯 GOAL(G)" "🚀 ACTIVE(A)" "⏸ PAUSED(x)" "|" "🏆 ACHIEVED(a)" "🚫 DROPPED(X)")))
  (setq org-todo-keyword-faces
        '(("☛ TODO" . (:foreground "#fb4934" :weight bold))
          ("⚡ NEXT" . (:foreground "#fabd2f" :weight bold))
          ("🔄 PROG" . (:foreground "#83a598" :weight bold))
          ("⏳ WAIT" . (:foreground "#d3869b" :weight bold))
          ("✅ DONE" . (:foreground "#b8bb26" :weight bold))
          ("❌ CANCELLED" . (:foreground "#928374" :weight bold))
          ("🎯 GOAL" . (:foreground "#b16286" :weight bold))
          ("🚀 ACTIVE" . (:foreground "#d79921" :weight bold))
          ("⏸ PAUSED" . (:foreground "#7c6f64" :weight bold))
          ("🏆 ACHIEVED" . (:foreground "#689d6a" :weight bold))
          ("🚫 DROPPED" . (:foreground "#665c54" :weight bold)))))

(after! org-modern
  (setq org-modern-star '("◉" "○" "◈" "◇" "◆" "▷")
        org-modern-hide-stars "· "
        org-modern-list '((43 . "➤") (45 . "–") (42 . "•"))
        org-modern-table-vertical 1
        org-modern-table-horizontal 0.1
        org-modern-block-name '(("src" "»" "«") ("example" "»" "«") ("quote" "❝" "❞"))
        ;; Style tags with a subtle box, inspired by Doom Emacs.
        org-modern-tag-faces `((:foreground ,(face-attribute 'default :foreground) :weight bold :box (:line-width (1 . -1) :color "#45475a")))
        ;; Prettier checkboxes
        org-modern-checkbox '((todo . "☐") (done . "☑") (cancel . "☒") (priority . "⚑") (on . "◉") (off . "○"))))

(after! org-appear
  (setq org-appear-autoemphasis t
        org-appear-autolinks t
        org-appear-autosubmarkers t))

(use-package! org-fragtog
  :hook (org-mode . org-fragtog-mode))

(after! org-capture
  (setq org-capture-templates
        (doct `(;; Main Capture Options
                ("Task" :keys "t"
                 :icon ("nf-oct-tasklist" :set "octicon" :color "red")
                 :file "inbox.org"
                 :headline "Tasks"
                 :template ("* ☛ TODO %?"
                            "  :PROPERTIES:"
                            "  :CREATED: %U"
                            "  :END:"))
                ("Note" :keys "n"
                 :icon ("nf-fa-sticky_note" :set "faicon" :color "yellow")
                 :file "inbox.org"
                 :headline "Notes"
                 :template ("* %? :note:"
                            "  :PROPERTIES:"
                            "  :CREATED: %U"
                            "  :END:"))
                ("Journal" :keys "j"
                 :icon ("nf-fa-calendar" :set "faicon" :color "pink")
                 :file "journal.org"
                 :datetree t
                 :template ("* %U %?"))
                ("Meeting" :keys "m"
                 :icon ("nf-mdi-account_group" :set "mdicon" :color "blue")
                 :file "inbox.org"
                 :headline "Meetings"
                 :template ("* Meeting: %? :meeting:"
                            "  :PROPERTIES:"
                            "  :CREATED: %U"
                            "  :ATTENDEES:"
                            "  :END:"
                            "** Agenda"
                            "** Notes"
                            "** Action Items"))
                ;; Long-term Planning
                ("Project" :keys "p"
                 :icon ("nf-oct-repo" :set "octicon" :color "green")
                 :file "projects.org"
                 :headline "Projects"
                 :template ("* 📋 PLAN %? :project:"
                            "  :PROPERTIES:"
                            "  :CREATED: %U"
                            "  :GOAL:"
                            "  :DEADLINE:"
                            "  :END:"
                            "** Goals"
                            "** Tasks"
                            "*** ☛ TODO Define project scope"
                            "** Resources"
                            "** Notes"))
                ("Book" :keys "b"
                 :icon ("nf-mdi-book_open_page_variant" :set "mdicon" :color "orange")
                 :file "reading.org"
                 :headline "Reading List"
                 :template ("* %? :book:read:"
                            "  :PROPERTIES:"
                            "  :CREATED: %U"
                            "  :AUTHOR:"
                            "  :GENRE:"
                            "  :RATING:"
                            "  :END:"
                            "** Summary"
                            "** Key Takeaways"
                            "** Quotes"))
                ("Goal" :keys "g"
                 :icon ("nf-mdi-flag_checkered" :set "mdicon" :color "purple")
                 :file "goals.org"
                 :headline "Goals"
                 :template ("* 🎯 GOAL %? :goal:"
                            "  DEADLINE: %(org-read-date nil nil \"+1y\")"
                            "  :PROPERTIES:"
                            "  :CREATED: %U"
                            "  :END:"
                            "** Why this goal?"
                            "** Success criteria"
                            "** Action steps"
                            "*** ☛ TODO Break down into smaller tasks"))
                ;; Protocol links
                ("Protocol" :keys "P"
                 :icon ("nf-fa-link" :set "faicon" :color "blue")
                 :file "Notes.org"
                 :template ("* ☛ TODO %^{Title}"
                            "Source: %u"
                            "#+BEGIN_QUOTE"
                            "%i"
                            "#+END_QUOTE"
                            "%?"))))))

(after! org-roam
  (setq org-roam-directory (expand-file-name "roam" org-directory))
  (setq org-roam-db-location (expand-file-name ".org-roam.db" org-roam-directory))

  (setq org-roam-node-display-template
        (concat "${title:*} " (propertize "${tags:20}" 'face 'org-tag)))

  ;; Configure the backlinks buffer to open on the right, like in vanilla config
  (add-to-list 'display-buffer-alist
               '("\\*org-roam\\*"
                 (display-buffer-in-direction)
                 (direction . right)
                 (window-width . 0.33)
                 (window-height . fit-window-to-buffer)))

  ;; Hook to update modification times, keeping the graph fresh
  (defun +my/org-roam-update-modified-timestamp ()
    "Update modified timestamp in org-roam files before saving."
    (when (and (eq major-mode 'org-mode) (org-roam-file-p))
      (save-excursion
        (goto-char (point-min))
        (when (re-search-forward "^#\\+modified:" nil t)
          (delete-region (point) (line-end-position))
          (insert (format " %s" (format-time-string "[%Y-%m-%d %a %H:%M]")))))))
  (add-hook 'before-save-hook #'+my/org-roam-update-modified-timestamp)
  (setq org-roam-dailies-directory "daily/"))

(use-package! org-roam-ui
  :after org-roam
  :config
  (setq org-roam-ui-sync-theme t
        org-roam-ui-follow t
        org-roam-ui-update-on-save t
        org-roam-ui-open-on-start nil))

(after! org-agenda
  (setq org-agenda-files (list org-directory (expand-file-name "roam" org-directory)))
  (setq org-agenda-skip-scheduled-if-done t
        org-agenda-skip-deadline-if-done t
        org-agenda-include-deadlines t
        org-agenda-block-separator 'hr
        org-agenda-compact-blocks t)
  (org-super-agenda-mode))

;; The powerful agenda "dashboard" from vanilla config
(setq org-agenda-custom-commands
      '(("o" "Dashboard"
         ((agenda "" ((org-deadline-warning-days 7)
                      (org-agenda-overriding-header "📅 Agenda")))
          (todo "⚡ NEXT" ((org-agenda-overriding-header "⚡ Next Tasks")))
          (tags-todo "project/🚀 ACTIVE" ((org-agenda-overriding-header "🚀 Active Projects")))
          (tags-todo "+PRIORITY=\"A\"" ((org-agenda-overriding-header "🔥 High Priority")))
          (todo "⏳ WAIT" ((org-agenda-overriding-header "⏳ Waiting On")))
          (tags-todo "+habit" ((org-agenda-overriding-header "🔄 Habits")))
          (stuck "" ((org-agenda-overriding-header "🚫 Stuck Projects")))))

        ("p" "Projects Overview"
         ((tags "project" ((org-agenda-overriding-header "📋 All Projects")))))

        ("g" "Goals Review"
         ((tags-todo "goal" ((org-agenda-overriding-header "🎯 Goals")))))))

(setq org-super-agenda-groups
      '((:name "🔥 Overdue" :deadline past)
        (:name "📅 Today" :time-grid t :scheduled today)
        (:name "⚡ Next" :todo "⚡ NEXT")
        (:name "🔴 Important" :priority "A")
        (:name "🚀 Active Projects" :tag "project" :todo "ACTIVE")
        (:name "🎯 Goals" :tag "goal")
        (:name "🔄 Habits" :tag "habit")
        (:name "⏳ Waiting" :todo "WAIT")
        (:discard (:anything t))))

(setq org-archive-location "archive/Archive_%s::")

(defun ar/org-archive-done-tasks ()
  "Attempt to archive all done tasks in file"
  (interactive)
  (org-map-entries
   (lambda ()
     (org-archive-subtree)
     (setq org-map-continue-from (org-element-property :begin (org-element-at-point))))
   "/DONE" 'file))

(map! :map org-mode-map :desc "Archive tasks marked DONE" "C-c DEL a" #'ar/org-archive-done-tasks)

(defun ar/org-remove-kill-tasks ()
  (interactive)
  (org-map-entries
   (lambda ()
     (org-cut-subtree)
     (pop kill-ring)
     (setq org-map-continue-from (org-element-property :begin (org-element-at-point))))
   "/KILL" 'file))

(map! :map org-mode-map :desc "Remove tasks marked as KILL" "C-c DEL k" #'ar/org-remove-kill-tasks)

(use-package! markdown-mode
  ;; These modes will run automatically whenever you open a markdown file.
  :hook ((markdown-mode . visual-line-mode)   ; Enable soft-wrapping for readability.
         (markdown-mode . markdown-toc-mode)) ; Enable table of contents commands.
  :config
  ;; Use grip for a GitHub-like preview in the browser.
  (setq markdown-command "grip"))

(use-package! vmd-mode
  :config
  ;; Configure the appearance of the live preview pane.
  ;; Other themes: 'gfm', 'neutron', 'classic'.
  (setq vmd-theme "github")
  (setq vmd-show-sidebar nil)) ; Hides the VMD navigation sidebar for a cleaner look.

(use-package! markdown-toc
  :config
  ;; Automatically update the Table of Contents when the file is saved.
  ;; This hook is smart and only runs if a TOC already exists.
  (add-hook 'before-save-hook #'markdown-toc-refresh-toc-in-buffer-only-if-dirty))

(map! :leader
      :map markdown-mode-map
      :prefix ("m" . "Markdown")
      ;; Live Previews
      "p" '(vmd-mode :wk "Toggle Live Preview Pane")
      "P" '(markdown-open :wk "Open in Browser (Grip)")

      ;; Table of Contents
      "t" '(:ignore t :wk "Table of Contents")
      "ti" '(markdown-toc-insert-toc :wk "Insert TOC")
      "tu" '(markdown-toc-refresh :wk "Update TOC")

      ;; Table Editing (using markdown-mode's built-in functions)
      "a" '(:ignore t :wk "Table")
      "aa" '(markdown-table-align :wk "Align/Format Table")
      "ar" '(markdown-table-insert-row :wk "Insert Row")
      "ac" '(markdown-table-insert-col :wk "Insert Column")
      "ad" '(markdown-table-delete-col :wk "Delete Column")

      ;; Insertion & Execution
      "i" '(:ignore t :wk "Insert")
      "ii" '(markdown-insert-image :wk "Insert Image")
      "il" '(markdown-insert-link :wk "Insert Link")
      "c" '(org-babel-execute-src-block :wk "Execute Code Block")

      ;; Navigation & Folding (uses evil-mode defaults)
      "]h" '(markdown-next-heading :wk "Next Heading")
      "[h" '(markdown-previous-heading :wk "Previous Heading")
      "]t" '(markdown-next-table :wk "Next Table")
      "[t" '(markdown-previous-table :wk "Previous Table"))

;;;; ------------------------------------------------------------------
;;;; PYTHON CONFIGURATION (FINALIZED)
;;;; ------------------------------------------------------------------

(use-package! python
  :config
  ;; Tree-sitter is automatically configured by the `+tree-sitter` flag
  ;; in your `init.el`. This provides superior syntax highlighting and code
  ;; navigation.

  ;;;###
  ;;;### Linter (Pylint) & Formatter (Black)
  ;;;###
  (setq flycheck-python-checkers '(pylint))
  (set-formatter! 'black '("black" "--quiet" "-") :modes '(python-mode))


  ;;;###
  ;;;### Interactive Evaluation with vterm
  ;;;###
  ;; Helper function to find a vterm buffer running a python/ipython REPL
  (defun ar/python-get-vterm-buffer ()
    "Find a vterm buffer running a python REPL, or create one."
    (let ((buf (cl-find-if (lambda (b)
                             (with-current-buffer b
                               (and (eq major-mode 'vterm-mode)
                                    (string-match-p "\\(ipython\\|python\\)" vterm--process-executable))))
                           (buffer-list))))
      (if buf
          buf
        (progn
          (call-interactively #'+vterm/here)
          (vterm-send-string "ipython\n")
          (current-buffer)))))

  ;; Function to send code to the vterm REPL
  (defun ar/python-send-to-vterm (beg end)
    "Send the region from BEG to END to a Python vterm REPL."
    (interactive "r")
    (with-current-buffer (ar/python-get-vterm-buffer)
      (vterm-send-string (buffer-substring-no-properties beg end))
      (vterm-send-string "\n")))


  ;;;###
  ;;;### Debugger (DAP for Python)
  ;;;###
  (after! dap-mode
    (dap-register-debug-template
     "Python: Debug Current File"
     (list :type "python" :request "launch" :name "DAP: Py - File"
           :program "${file}" :console "integratedTerminal"))

    (dap-register-debug-template
     "Python: Debug File w/ Args"
     (list :type "python" :request "launch" :name "DAP: Py - File w/ Args"
           :program "${file}" :args (split-string (read-string "Arguments: "))
           :console "integratedTerminal"))

    (dap-register-debug-template
     "Python: Debug Pytest File"
     (list :type "python" :request "launch" :name "DAP: Py - Pytest File"
           :module "pytest" :args ["-s" "-v" "${file}"]
           :console "integratedTerminal"))

    (dap-register-debug-template
     "Python: Debug Pytest Project"
     (list :type "python" :request "launch" :name "DAP: Py - Pytest Project"
           :module "pytest" :args ["-s" "-v"]
           :console "integratedTerminal"))

    (dap-register-debug-template
     "Python: Attach to Process"
     (list :type "python" :request "attach" :name "DAP: Py - Attach"
           :connect (list :host "localhost"
                          :port (read-string "Attach to port: " "5678")))))


  ;;;###
  ;;;### Org Mode, Jupyter, and Environment Management
  ;;;###
  ;; For Poetry/Venv projects, ensure Emacs uses the correct Python environment.
  ;; Since you enabled `:tools direnv` and `:lang python +poetry`, the best way
  ;; is to `cd` into your project and run `direnv allow` in your shell. This
  ;; will automatically activate the poetry virtualenv for Emacs, so that
  ;; `pyright` (LSP) and `jupyter` find the correct packages.
  (after! org
    (org-babel-do-load-languages
     'org-babel-load-languages
     '((python . t) (jupyter . t)))

    (add-hook 'org-src-mode-hook
              (defun ar/org-python-src-block-setup ()
                "Enable Python IDE features in Org source blocks."
                (when (string-equal-nocase (org-element-property :language (org-element-at-point))
                                           "python")
                  (lsp-deferred)
                  (eldoc-mode +1)))))


  ;;;###
  ;;;### Keybindings
  ;;;###
  (map! :leader
        :map python-mode-map
        ;; Code (c) - Run / Send
        :prefix ("c" . "code")
        "c" '(python-execute-file :wk "Run file")
        "r" '(dap-python-run-repl :wk "Run project REPL")
        "s" '(:ignore t :wk "send to vterm")
        "sl" '(ar/python-send-to-vterm :wk "Send line")
        "sr" #'(lambda () (interactive) (ar/python-send-to-vterm (region-beginning) (region-end)))
        ;; Debugging (d)
        :prefix ("d" . "debug")
        "d" '(dap-debug :wk "Debug...")
        "b" '(dap-toggle-breakpoint :wk "Toggle breakpoint")
        "c" '(dap-continue :wk "Continue")
        "q" '(dap-disconnect :wk "Disconnect")
        ;; Testing (t)
        :prefix ("t" . "test")
        "f" '(dap-pytest-run :wk "Run tests in file")
        "p" #'(lambda () (interactive) (dap-debug "Python: Debug Pytest Project"))))

(defvar my-bib-file (expand-file-name "roam/bibliography.bib" org-directory)
  "The absolute path to the bibliography file auto-exported by Zotero.")

(after! citar
  (setq citar-bibliography (list my-bib-file))
  (setq citar-notes-paths (list (expand-file-name "roam/notes/" org-directory)))
  (setq citar-at-point-function 'embark-act) ; Use Embark for contextual actions

  ;; Enable embark integration for citar
  (citar-embark-mode)

  (setq citar-symbols
        `((file ,(nerd-icons-octicon "nf-oct-file" :face 'nerd-icons-red) . " ")
          (note ,(nerd-icons-octicon "nf-oct-note" :face 'nerd-icons-yellow) . " ")
          (link ,(nerd-icons-octicon "nf-oct-link" :face 'nerd-icons-blue) . " "))))

(after! org-roam
  (require 'citar-org-roam)
  (citar-org-roam-mode))

(after! tex
  (setq-default TeX-engine 'tectonic)
  (setq TeX-view-program-selection '((output-pdf "PDF Tools")))
  (setq TeX-source-correlate-mode t)
  (setq TeX-source-correlate-start-server t)
  (setq font-latex-fontify-sectioning 1.3) ; Scale section headers for readability

  (defun my/tex-compile-and-clean ()
    "Compile the LaTeX file with Tectonic and clean auxiliary files on success."
    (interactive)
    (let ((TeX-clean-extensions
           '(".aux" ".bbl" ".blg" ".log" ".out" ".toc" ".fls" ".fdb_latexmk" "*-blx.bib" "*.run.xml")))
      (TeX-command-master nil (lambda () (TeX-clean)))))

  (add-hook 'LaTeX-mode-hook
            (lambda ()
              (outline-minor-mode)
              (rainbow-delimiters-mode))))

(after! lsp-mode
  ;; Register texlab as a client for lsp-mode
  (lsp-register-client
   (make-lsp-client :new-connection (lsp-stdio-connection '("texlab"))
                    :major-modes '(tex-mode latex-mode)
                    :remote? t
                    :server-id 'texlab))
  ;; Configure lsp-mode's settings for the texlab client
  (setf (lsp--client-const (lsp-client-by-id 'texlab) :settings)
        (lambda ()
          `(:texlab
            (:build
             (:executable "tectonic")
             (:args ["-Z" "shell-escape" "--outdir=%OUTDIR%" "%FILE%"]))
            (:forwardSearch
             (:executable "zathura")
             (:args ["--synctex-forward" "%LINE%:%COLUMN%" "%PDF%"]))))))

(setq +zen-mixed-pitch-modes '(org-mode LaTeX-mode markdown-mode))
(dolist (hook +zen-mixed-pitch-modes)
  (add-hook (intern (concat (symbol-name hook) "-hook")) #'mixed-pitch-mode))
(add-hook 'org-mode-hook #'org-fragtog-mode)
(after! laas (add-hook 'LaTeX-mode-hook #'laas-mode))

(after! yasnippet
  (let* (;; --- Source Lists for Snippet Generation ---
         (greek-alphabet
          '(("a" . "alpha") ("b" . "beta") ("g" . "gamma") ("d" . "delta")
            ("e" . "epsilon") ("z" . "zeta") ("h" . "eta") ("th" . "theta")
            ("i" . "iota") ("k" . "kappa") ("l" . "lambda") ("m" . "mu")
            ("n" . "nu") ("x" . "xi") ("p" . "pi") ("r" . "rho")
            ("s" . "sigma") ("t" . "tau") ("u" . "upsilon") ("ph" . "phi")
            ("ch" . "chi") ("ps" . "psi") ("o" . "omega")))
         (math-symbols
          '(("!=" . "neq") (">=" . "geq") ("<=" . "leq") ("->" . "to")
            ("<-" . "leftarrow") ("=>" . "Rightarrow") ("<=" . "Leftarrow")
            ("v" . "forall") ("e" . "exists") ("!e" . "nexists")
            ("in" . "in") ("!in" . "notin") ("sub" . "subset") ("sup" . "supset")
            ("sube" . "subseteq") ("supe" . "supseteq") ("0" . "emptyset")
            ("inf" . "infty") ("d" . "partial") ("grad" . "nabla")))
         (math-environments
          '(("eq" . "equation") ("eq*" . "equation*") ("ali" . "align")
            ("ali*" . "align*") ("gat" . "gather") ("gat*" . "gather*")))
         (math-structures
          '(("f" . "\\frac{$1}{$2}$0") ("sq" . "\\sqrt{$1}$0")
            ("sqr" . "\\sqrt[$2]{$1}$0") ("hat" . "\\hat{$1}$0")
            ("bar" . "\\bar{$1}$0") ("vec" . "\\vec{$1}$0") ("til" . "\\tilde{$1}$0")
            ("dot" . "\\dot{$1}$0") ("ddot" . "\\ddot{$1}$0")))
         (section-commands
          '(("ch" . "chapter") ("sec" . "section") ("ssec" . "subsection")
            ("sssec" . "subsubsection") ("par" . "paragraph")))
         (theorem-environments
          '(("thm" . "theorem") ("lem" . "lemma") ("cor" . "corollary")
            ("prop" . "proposition") ("defn" . "definition") ("rem" . "remark"))))

    ;; --- Snippet Definition Logic ---
    (yas-define-snippets 'latex-mode
      (append
       ;; Greek letters (e.g., 'a -> \alpha, 'A -> \Alpha)
       (mapcan (lambda (g)
                 `((,(concat "'" (car g)) ,(concat "\\" (cdr g)))
                   (,(concat "'" (upcase (car g))) ,(concat "\\" (capitalize (cdr g))))))
               greek-alphabet)
       ;; Math blackboard bold (e.g., `R -> \mathbb{R})
       (mapcar (lambda (c) `(,(concat "`" c) ,(concat "\\mathbb{" c "}")))
               '("R" "C" "N" "Z" "Q" "P" "E" "F" "H" "I" "K" "L" "S" "T"))
       ;; General math symbols (e.g., ;!= -> \neq)
       (mapcar (lambda (m) `(,(concat ";" (car m)) ,(concat "\\" (cdr m)))) math-symbols)
       ;; Math environments (e.g., ,eq -> \begin{equation}...)
       (mapcar (lambda (e) `(,(concat "," (car e))
                              ,(format "\\begin{%s}\n  $0\n\\end{%s}" (cdr e) (cdr e))))
               math-environments)
       ;; Math structures (e.g., //f -> \frac{}{})
       (mapcar (lambda (s) `(,(concat "//" (car s)) ,(cdr s))) math-structures)
       ;; Sectioning commands with labels (e.g., sec -> \section{}, secl -> \section{} \label{})
       (mapcan (lambda (s)
                 (let* ((key (car s)) (cmd (cdr s)))
                   `((,key ,(format "\\%s{$1}$0" cmd))
                     (,(concat key "l") ,(format "\\%s{$1} \\label{%s:$2}\n$0" cmd key)))))
               section-commands)
       ;; Theorem-like environments (e.g., Bthm -> \begin{theorem})
       (mapcar (lambda (e) `(,(concat "B" (car e)) ,(format "\\begin{%s}\n  $0\n\\end{%s}" (cdr e) (cdr e))))
               theorem-environments)))))

(defun my/latex-find-project-packages ()
  "Find all .sty files in the project's 'styles' directory or parent directories."
  (let ((search-dirs '("./styles/" "../styles/" "./" "../")))
    (seq-uniq
     (seq-sort #'string-lessp
               (seq-filter #'identity
                           (mapcan (lambda (dir)
                                     (when (file-directory-p dir)
                                       (directory-files dir t "\\.sty$")))
                                   search-dirs))))))

(defun my/latex-insert-project-packages (format-str)
  "Insert project .sty files using FORMAT-STR."
  (let ((packages (my/latex-find-project-packages)))
    (if (not packages)
        (message "No local .sty files found.")
      (insert (string-join (mapcar (lambda (file)
                                     (format format-str
                                             (file-name-sans-extension
                                              (file-relative-name file default-directory))))
                                   packages)
                           "\n")))))

(defun my/latex-insert-packages-tex ()
  "Insert \\usepackage lines for local .sty files."
  (interactive)
  (my/latex-insert-project-packages "\\usepackage{%s}"))

(defun my/latex-insert-packages-org ()
  "Insert #+LATEX_HEADER lines for local .sty files."
  (interactive)
  (my/latex-insert-project-packages "#+LATEX_HEADER: \\usepackage{%s}"))

(after! ox-latex
  (setq org-latex-listings 'engraved)
  (setq org-latex-pdf-process '("tectonic -Z shell-escape --outdir=%o %f"))
  (setq org-latex-default-class "chameleon")
  (setq org-beamer-theme "[progressbar=foot]metropolis")

  (defvar my/org-latex-features
    '(("\\[\\[\\(?:file\\|https?\\):[^]]+?\\.\\(?:eps\\|pdf\\|png\\|jpeg\\|jpg\\)\\]\\]" . image)
      ("^[ \t]*|" . table)
      ("cref:" . cleveref)
      ("^[ \t]*#\\+begin_(?:warning|info|success|error)" . tcolorbox)
      ((lambda (info) (eq 'beamer (org-export-backend-name (plist-get info :back-end)))) . beamer)))

  (defvar my/org-latex-feature-pkgs
    '((image . "\\usepackage{graphicx}")
      (table . "\\usepackage{longtable}\n\\usepackage{booktabs}")
      (cleveref . "\\usepackage[capitalize]{cleveref}")
      (tcolorbox . "\\usepackage[most]{tcolorbox}")
      (beamer . "\\usepackage{scrextend}")))

  (defun my/org-latex-get-preamble (info)
    (mapconcat (lambda (feature) (cdr (assq feature my/org-latex-feature-pkgs)))
               (delete-dups
                (mapcan (lambda (ft)
                          (when (pcase (car ft)
                                  ((pred stringp) (save-excursion (re-search-forward (car ft) nil t)))
                                  ((pred functionp) (funcall (car ft) info)))
                            (if (listp (cdr ft)) (cdr ft) (list (cdr ft)))))
                        my/org-latex-features))
               "\n"))

  (defvar-local my/org-latex-info-buffer nil)
  (advice-add #'org-latex-make-preamble :before
              (defun my/org-latex-save-info-advice (info &rest _)
                (setq my/org-latex-info-buffer info)))

  (advice-add #'org-splice-latex-header :around
              (defun my/org-splice-latex-header-advice (orig-fn &rest args)
                (let ((header (apply orig-fn args)))
                  (if (plist-get (car (last args)) :latex-snippets) header
                    (concat header "\n%% Dynamic Preamble\n"
                            (my/org-latex-get-preamble my/org-latex-info-buffer)
                            "\n%% End Dynamic Preamble\n")))))

  (add-to-list 'org-latex-classes
               '("chameleon"
                 "\\documentclass[11pt,a4paper]{scrartcl}
[PACKAGES]
[DEFAULT-PACKAGES]
[EXTRA]
\\usepackage{fontspec}
\\usepackage{geometry}
\\usepackage[svgnames]{xcolor}
\\usepackage{hyperref}
\\setmainfont{Source Serif Pro}
\\setmonofont{JetBrains Mono}[Scale=MatchLowercase]
\\usepackage{microtype}
\\geometry{margin=1in}
\\hypersetup{colorlinks=true, linkcolor=NavyBlue, citecolor=ForestGreen, urlcolor=SteelBlue}
% Support for admonition boxes
\\newtcolorbox{warning}{colback=yellow!10,colframe=yellow!70!black,title=Warning}
\\newtcolorbox{info}{colback=blue!10,colframe=blue!70!black,title=Info}
\\newtcolorbox{success}{colback=green!10,colframe=green!70!black,title=Success}
\\newtcolorbox{error}{colback=red!10,colframe=red!70!black,title=Error}"
                 ("\\section{%s}" . "\\section*{%s}"))))

(map! :leader
      :map latex-mode-map
      :prefix ("m" . "latex")
      "c" '(:ignore t :wk "Compile")
      "cc" '(my/tex-compile-and-clean :wk "Compile & Clean")
      "cv" '(TeX-view :wk "View Output")
      "ce" '(TeX-error-overview :wk "Error Overview")
      "ck" '(TeX-clean :wk "Clean Aux Files")
      "i" '(:ignore t :wk "Insert")
      "ic" '(citar-insert-citation :wk "Insert Citation")
      "ip" '(my/latex-insert-packages-tex :wk "Insert Project Packages")
      "il" '(LaTeX-insert-label :wk "Insert Label")
      "ir" '(LaTeX-insert-ref :wk "Insert Reference")
      "e" '(:ignore t :wk "Environment")
      "ee" '(LaTeX-environment :wk "Insert Environment")
      "s" '(:ignore t :wk "Section")
      "ss" '(LaTeX-section :wk "Insert Section"))

(map! :leader
      :map org-mode-map
      :prefix ("m" . "org")
      "p" '(my/latex-insert-packages-org :wk "Insert Project Packages"))

(setq forge-owned-accounts '(("aahsnr")))

(use-package! feature-mode
  :mode "\\.feature$")

(use-package! systemd
  :mode "\\.service$")

;; (use-package! rpm-spec-mode
;;   :mode "\\.spec\\(\\.in\\)?$")

(map! :leader
      :desc "Open like spacemacs" "SPC" #'execute-extended-command)
```

# init.el

```el
;;; init.el -*- lexical-binding: t; -*-

;; This file controls what Doom modules are enabled and what order they load
;; in. Remember to run 'doom sync' after modifying it!

;; NOTE Press 'SPC h d h' (or 'C-h d h' for non-vim users) to access Doom's
;;      documentation. There you'll find a link to Doom's Module Index where all
;;      of our modules are listed, including what flags they support.

;; NOTE Move your cursor over a module's name (or its flags) and press 'K' (or
;;      'C-c c k' for non-vim users) to view its documentation. This works on
;;      flags as well (those symbols that start with a plus).
;;
;;      Alternatively, press 'gd' (or 'C-c c d') on a module to browse its
;;      directory (for easy access to its source code).

(doom! :input
       ;;bidi              ; (tfel ot) thgir etirw uoy gnipleh
       ;;chinese
       ;;japanese
       ;;layout            ; auie,ctsrnm is the superior home row

       :completion
       ;;company           ; the ultimate code completion backend
       (corfu
        +icons
        +orderless
        +dabbrev)
       ;;helm              ; the *other* search engine for love and life
       ;;ido               ; the other *other* search engine...
       ;;ivy               ; a search engine for love and life
       (vertico
        +icons)

       :ui
       ;;deft              ; notational velocity for Emacs
       doom                ; what makes DOOM look the way it does
       doom-dashboard      ; a nifty splash screen for Emacs
       ;;doom-quit         ; DOOM quit-message prompts when you quit Emacs
       (emoji +unicode)    ; 🙂
       ;;hl-todo           ; highlight TODO/FIXME/NOTE/DEPRECATED/HACK/REVIEW
       indent-guides       ; highlighted indent columns
       (ligatures
        +extra)
       minimap           ; show a map of the code on the side
       modeline            ; snazzy, Atom-inspired modeline, plus API
       ;;nav-flash         ; blink cursor line after big motions
       ;;neotree           ; a project drawer, like NERDTree for vim
       ophints             ; highlight the region an operation acts on
       (popup
        +all
        +defaults)
       (smooth-scroll
        +interpolate)     ; So smooth you won't believe it's not butter
       ;;tabs
       (treemacs
        +lsp)
       ;;unicode           ; extended unicode support for various languages
       (vc-gutter +pretty) ; vcs diff in the fringe
       ;;vi-tilde-fringe   ; fringe tildes to mark beyond EOB
       window-select       ; visually switch windows
       workspaces          ; tab emulation, persistence & separate workspaces
       zen                 ; distraction-free coding or writing

       :editor
       (evil +everywhere)  ; come to the dark side, we have cookies
       file-templates      ; auto-snippets for empty files
       fold                ; (nigh) universal code folding
       (format
        +onsave
        +lsp)
       ;;god               ; run Emacs commands without modifier keys
       ;;lispy             ; vim for lisp, for people who don't like vim
       ;;multiple-cursors  ; editing in many places at once
       ;;objed             ; text object editing for the innocent
       parinfer          ; turn lisp into python, sort of
       ;;rotate-text       ; cycle region at point between text candidates
       snippets            ; my elves. They type so I don't have to
       word-wrap           ; soft wrapping with language-aware indent

       :emacs
       (dired
        +dirvish
        +icons)
       electric            ; smarter, keyword-based electric-indent
       ;;eww               ; the internet is gross
       (ibuffer
        +icons)
       undo              ; persistent, smarter undo for your inevitable mistakes
       vc                ; version-control and Emacs, sitting in a tree

       :term
       ;;eshell            ; the elisp shell that works everywhere
       ;;shell             ; simple shell REPL for Emacs
       ;;term              ; basic terminal emulator for Emacs
       vterm             ; the best terminal emulation in Emacs

       :checkers
       (syntax
        +childframe
        +icons)
       ;;grammar           ; tasing grammar mistake every you make

       :tools
       ansible
       biblio              ; Writes a PhD for you (citation needed)
       ;;collab            ; buffers with friends
       (debugger
        +lsp)
       direnv
       ;;docker
       ;;editorconfig      ; let someone else argue about tabs vs spaces
       ;;ein               ; tame Jupyter notebooks with emacs
       (eval +overlay)     ; run code, run (also, repls)
       lookup              ; navigate your code and its documentation
       ;;llm               ; when I said you needed friends, I didn't mean...
       (lsp
        +peek)
       (magit
        +forge)
       ;;make              ; run make tasks from Emacs
       ;;pass              ; password manager for nerds
       pdf                 ; pdf enhancements
       ;;terraform         ; infrastructure as code
       tmux                ; an API for interacting with tmux
       tree-sitter         ; syntax and parsing, sitting in a tree...
       ;;upload            ; map local to remote projects via ssh/ftp

       :os
       (tty
        +osc)

       :lang
       ;;agda              ; types of types of types of types...
       ;;beancount         ; mind the GAAP
       (cc
        +lsp
        +tree-sitter)
       ;;clojure           ; java with a lisp
       ;;common-lisp       ; if you've seen one lisp, you've seen them all
       ;;coq               ; proofs-as-programs
       ;;crystal           ; ruby at the speed of c
       ;;csharp            ; unity, .NET, and mono shenanigans
       ;;data              ; config/data formats
       ;;(dart +flutter)   ; paint ui and not much else
       ;;dhall
       ;;elixir            ; erlang done right
       ;;elm               ; care for a cup of TEA?
       emacs-lisp        ; drown in parentheses
       ;;erlang            ; an elegant language for a more civilized age
       ;;ess               ; emacs speaks statistics
       ;;factor
       ;;faust             ; dsp, but you get to keep your soul
       ;;fsharp            ; ML stands for Microsoft's Language
       ;;fstar             ; (dependent) types and (monadic) effects and Z3
       ;;gdscript          ; the language you waited for
       ;;(go +lsp)         ; the hipster dialect
       ;;(graphql +lsp)    ; Give queries a REST
       ;;(haskell +lsp)    ; a language that's lazier than I am
       ;;hy                ; readability of scheme w/ speed of python
       ;;idris             ; a language you can depend on
       ;;json              ; At least it ain't XML
       ;;janet             ; Fun fact: Janet is me!
       ;;(java +lsp)       ; the poster child for carpal tunnel syndrome
       ;;javascript        ; all(hope(abandon(ye(who(enter(here))))))
       ;;julia             ; a better, faster MATLAB
       ;;kotlin            ; a better, slicker Java(Script)
       (latex
        +cdlatex
        +fold
        +lsp)
       ;;lean              ; for folks with too much to prove
       ;;ledger            ; be audit you can be
       ;;lua               ; one-based indices? one-based indices
       markdown
       ;;nim               ; python + lisp at the speed of c
       (nix
        +tree-sitter
        +lsp)
       ;;ocaml             ; an objective camel
       (org
        +dragndrop
        +gnuplot
        +jupyter
        +noter
        +pandoc
        +pretty
        +roam2)
       ;;php               ; perl's insecure younger brother
       ;;plantuml            ; diagrams for confusing people more
       ;;graphviz            ; diagrams for confusing yourself even more
       ;;purescript        ; javascript, but functional
       (python
        +lsp
        +pyright
        +tree-sitter)
       ;;qt                ; the 'cutest' gui framework ever
       ;;racket            ; a DSL for DSLs
       ;;raku              ; the artist formerly known as perl6
       ;;rest              ; Emacs as a REST client
       ;;rst               ; ReST in peace
       ;;(ruby +rails)     ; 1.step {|i| p "Ruby is #{i.even? ? 'love' : 'life'}"}
       ;;(rust +lsp)       ; Fe2O3.unwrap().unwrap().unwrap().unwrap()
       ;;scala             ; java, but good
       ;;(scheme +guile)   ; a fully conniving family of lisps
       (sh
        +fish
        +lsp
        +tree-sitter)
       ;;sml
       ;;solidity          ; do you need a blockchain? No.
       ;;swift             ; who asked for emoji variables?
       ;;terra             ; Earth and Moon in alignment for performance.
       ;;web               ; the tubes
       ;;yaml              ; JSON, but readable
       ;;zig               ; C, but simpler

       :email
       ;;(mu4e +org +gmail)
       ;;notmuch
       ;;(wanderlust +gmail)

       :app
       ;;calendar
       ;;emms
       ;;everywhere        ; *leave* Emacs!? You must be joking
       ;;irc               ; how neckbeards socialize
       ;;(rss +org)        ; emacs as an RSS reader

       :config
       literate
       (default +bindings +smartparens))

```

# packages.el

```el
;;; -*- lexical-binding: t; -*-

;; Theme & UI
(package! rainbow-identifiers)
(package! rainbow-delimiters)

;; Evil

;; Org & Roam
(package! org-roam-ui)
(package! doct)
(package! org-super-agenda)

;; Spell Checking
(package! jinx)

;; Completion & Snippets
(package! consult-yasnippet)

;; Citations & LaTeX
(package! consult-bibtex
  :recipe (:host github :repo "mohkale/consult-bibtex"))
(package! laas)
(package! ox-chameleon :recipe (:host github :repo "tecosaur/ox-chameleon"))
(package! engrave-faces :recipe (:host github :repo "tecosaur/engrave-faces"))

;; Development & System
(package! embark-vc)
(package! feature-mode)
(package! sideline-flycheck)

;; Ignored Packages
(package! hydra :ignore t)
(package! helm-bibtex :ignore t)
(package! flycheck-popup-tip :ignore t)
(package! flycheck-posframe :ignore t)
```
