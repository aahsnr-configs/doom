```el
;; --- Variables ---
(defvar my/org-directory "~/org/" "Base directory for all org files.")
(defvar my/org-roam-directory (expand-file-name "roam/" my/org-directory) "Directory for org-roam files.")

;; --- Org Mode General Configuration ---
(after! org
  (setq org-directory my/org-directory
        org-agenda-files '("~/org/inbox.org" "~/org/projects.org" "~/org/habits.org" "~/org/goals.org")
        org-default-notes-file (expand-file-name "inbox.org" my/org-directory)
        org-archive-location (concat (file-name-as-directory (expand-file-name "archive" my/org-directory)) "Archive_%s::")

        ;; Performance Tweak: org-auto-align-tags can be slow in large files.
        org-auto-align-tags nil
        org-hide-emphasis-markers t

        ;; Performance Tweak: Inline images at startup can slow down Emacs start if the initial org file is large.
        ;; Consider setting to nil if startup is slow.
        org-startup-with-inline-images t
        org-image-actual-width 600)

  (add-hook! 'org-mode-hook #'ar/org-setup-hook))

;; --- Org Mode UI and Fonts ---
(defun ar/org-font-setup ()
  "Set fonts for Org headings and faces."
  (dolist (face '((org-level-1 . 1.2) (org-level-2 . 1.1) (org-level-3 . 1.05)
                  (org-level-4 . 1.0) (org-level-5 . 1.1) (org-level-6 . 1.1)
                  (org-level-7 . 1.1) (org-level-8 . 1.1)))
    (set-face-attribute (car face) nil :font "JetBrainsMono Nerd Font" :weight 'bold :height (cdr face) :slant 'unspecified))
  (set-face-attribute 'org-tag nil :foreground nil :inherit '(shadow fixed-pitch) :weight 'bold)
  (set-face-attribute 'org-block nil :foreground nil :inherit 'fixed-pitch)
  (set-face-attribute 'org-code nil :inherit '(shadow fixed-pitch))
  (set-face-attribute 'org-table nil :inherit '(shadow fixed-pitch))
  (set-face-attribute 'org-verbatim nil :inherit '(shadow fixed-pitch))
  (set-face-attribute 'org-special-keyword nil :inherit '(font-lock-comment-face fixed-pitch))
  (set-face-attribute 'org-meta-line nil :inherit '(font-lock-comment-face fixed-pitch))
  (set-face-attribute 'org-checkbox nil :inherit 'fixed-pitch))

(defun ar/org-setup-hook ()
  "Modes to enable on org-mode start."
  (org-indent-mode)
  (visual-line-mode 1)
  (ar/org-font-setup))

;; --- TODO Keywords ---
(after! org
  (setq org-todo-keywords
        '((sequence "📥 TODO(t)" "⚡ NEXT(n)" "⚙️ PROG(p)" "⏳ WAIT(w@/!)" "|" "✅ DONE(d!)" "❌ CANCEL(c@)")
          (sequence "📝 PLAN(P)" "🚀 ACTIVE(A)" "⏸️ PAUSED(x)" "|" "🏆 ACHIEVED(a)" "🗑️ DROPPED(D)")))
  (setq org-todo-keyword-faces
        '(("📥 TODO" . (:foreground ,(doom-color 'red) :weight bold))
          ("⚡ NEXT" . (:foreground ,(doom-color 'orange) :weight bold))
          ("⚙️ PROG" . (:foreground ,(doom-color 'blue) :weight bold))
          ("⏳ WAIT" . (:foreground ,(doom-color 'yellow) :weight bold))
          ("✅ DONE" . (:foreground ,(doom-color 'green) :weight bold))
          ("❌ CANCEL" . (:foreground ,(doom-color 'comment) :weight bold))
          ("📝 PLAN" . (:foreground ,(doom-color 'cyan) :weight bold))
          ("🚀 ACTIVE" . (:foreground ,(doom-color 'magenta) :weight bold))
          ("⏸️ PAUSED" . (:foreground ,(doom-color 'fg) :weight bold))
          ("🏆 ACHIEVED" . (:foreground ,(doom-color 'green) :weight bold))
          ("🗑️ DROPPED" . (:foreground ,(doom-color 'comment) :weight bold)))))

;; --- Package Configurations ---
(use-package! org-tempo
  :after org
  :config
  (add-to-list 'org-structure-template-alist '("sh" . "src shell"))
  (add-to-list 'org-structure-template-alist '("py" . "src python"))
  (add-to-list 'org-structure-template-alist '("el" . "src emacs-lisp")))

(use-package! org-fragtog :hook (org-mode . org-fragtog-mode))

(after! org-modern
  (setq org-modern-star '("◉" "○" "◈" "◇" "◆" "▷")
        org-modern-hide-stars "· "
        org-modern-list '((43 . "➤") (45 . "–") (42 . "•"))
        org-modern-table-vertical 1
        org-modern-table-horizontal 0.1
        org-modern-block-name '(("src" "»" "«") ("example" "»" "«") ("quote" "❝" "❞"))
        org-modern-tag-faces `((:foreground ,(face-attribute 'default :foreground) :weight bold :box (:line-width (1 . -1) :color "#45475a")))
        org-modern-checkbox '((todo . "☐") (done . "☑") (cancel . "☒") (priority . "⚑") (on . "◉") (off . "○"))))

(after! org-appear
  (setq org-appear-autoemphasis t
        org-appear-autolinks t
        org-appear-autosubmarkers t))

;; --- Org Capture ---
(defun ar/find-org-projects ()
  "Return a list of all Org files with a 'project' tag for capture."
  (let* ((builder (consult--grep-builder
                   (list consult-ripgrep-args
                         "--files-with-matches"
                         "--glob=*.org"
                         "^#\\+filetags:.*:project:.*"
                         (expand-file-name my/org-directory)))))
    (mapcar (lambda (file)
              (list (file-name-nondirectory file) file))
            (consult--grep-sync builder))))

(setf (alist-get 'height +org-capture-frame-parameters) 15)
(after! org-capture
  (setq org-capture-templates
        '(("t" "📥 Task" entry (file+headline "~/org/inbox.org" "Tasks")
           "* 📥 TODO %?\n  :PROPERTIES:\n  :CREATED: %U\n  :END:\n")
          ("n" "📝 Note" entry (file+headline "~/org/inbox.org" "Notes")
           "* %? :note:\n  :PROPERTIES:\n  :CREATED: %U\n  :SOURCE: \n  :END:\n")
          ("j" "📔 Journal" entry (file+olp+datetree "~/org/journal.org")
           "* %U %?\n")
          ("m" "🤝 Meeting" entry (file+headline "~/org/inbox.org" "Meetings")
           "* Meeting: %? :meeting:\n  :PROPERTIES:\n  :CREATED: %U\n  :ATTENDEES: \n  :END:\n** Agenda\n** Notes\n** Action Items\n")
          ("p" "📝 Project" entry (file+headline "~/org/projects.org" "Projects")
           "* 📝 PLAN %? :project:\n  :PROPERTIES:\n  :CREATED: %U\n  :GOAL: \n  :DEADLINE: \n  :END:\n** Goals\n** Tasks\n*** 📥 TODO Define project scope\n** Resources\n** Notes\n")
          ("P" "📌 Project Task" entry
           (file (lambda ()
                   (let* ((project-list (ar/find-org-projects))
                          (project-name (completing-read "Select Project: " project-list)))
                     (cdr (assoc project-name project-list)))))
           "* 📥 TODO %?\n  :PROPERTIES:\n  :CREATED: %U\n  :END:\n"
           :prepend t
           :headline "Tasks")
          ("b" "📚 Book" entry (file+headline "~/org/reading.org" "Reading List")
           "* %? :book:read:\n  :PROPERTIES:\n  :CREATED: %U\n  :AUTHOR: \n  :GENRE: \n  :PAGES: \n  :STARTED: \n  :FINISHED: \n  :RATING: \n  :END:\n** Summary\n** Key Takeaways\n** Quotes\n")
          ("h" "🔄 Habit" entry (file+headline "~/org/habits.org" "Habits")
           "* 📥 TODO %? :habit:\n  SCHEDULED: %(format-time-string \"%<<%Y-%m-%d %a .+1d>>\")\n  :PROPERTIES:\n  :CREATED: %U\n  :STYLE: habit\n  :END:\n")
          ("g" "🎯 Goal" entry (file+headline "~/org/goals.org" "Goals")
           "* 🎯 GOAL %? :goal:\n  DEADLINE: %(org-read-date nil nil \"+1y\")\n  :PROPERTIES:\n  :CREATED: %U\n  :TYPE: \n  :END:\n** Why this goal?\n** Success criteria\n** Action steps\n*** 📥 TODO Break down into smaller tasks\n** Resources needed\n** Potential obstacles\n** Progress tracking\n"))))

;; --- Org Roam ---
(after! org-roam
  (setq org-roam-directory my/org-roam-directory
        org-roam-db-location (expand-file-name ".org-roam.db" org-roam-directory)
        org-roam-node-display-template (concat "${title:*} " (propertize "${tags:20}" 'face 'org-tag))
        org-roam-capture-templates
        '(("d" "default" plain "* %?"
           :target (file+head "${slug}.org"
                              "#+title: ${title}\n#+filetags: \n\n")
           :unnarrowed t)
          ("p" "project" plain "* Goal\n\n%?\n\n* Tasks\n\n* Notes\n\n* Log\n"
           :target (file+head "projects/${slug}.org"
                              "#+title: Project: ${title}\n#+filetags: project\n")
           :unnarrowed t)
          ("l" "literature note" plain "* Source\n  - Author: \n  - Title: \n  - Year: \n\n* Summary\n\n%?\n\n* Key Takeaways\n\n* Quotes\n"
           :target (file+head "literature/${slug}.org"
                              "#+title: ${title}\n#+filetags: literature\n")
           :unnarrowed t)
          ("i" "idea" plain "* %?"
           :target (file+head "ideas/${slug}.org"
                              "#+title: ${title}\n#+filetags: idea fleeting\n")
           :unnarrowed t)
          ("z" "zettel" plain "* %?\n\n* References\n\n"
           :target (file+head "zettel/${slug}.org"
                              "#+title: ${title}\n#+filetags: zettel permanent\n")
           :unnarrowed t)
          ("j" "journal" plain "* Log\n\n%?"
           :target (file+olp+datetree (expand-file-name "journal.org" my/org-roam-directory))
           :unnarrowed t)))
  (add-to-list 'display-buffer-alist
               '("\\*org-roam\\*"
                 (display-buffer-in-direction)
                 (direction . right)
                 (window-width . 0.33)
                 (window-height . fit-window-to-buffer))))

(use-package! org-roam-ui
  :after org-roam
  :commands org-roam-ui-mode
  :config
  (setq org-roam-ui-sync-theme t
        org-roam-ui-follow t
        org-roam-ui-update-on-save t
        org-roam-ui-open-on-start nil))

(use-package! consult-org-roam
  :after (consult org-roam)
  :config (consult-org-roam-mode 1))


;; --- Org Agenda ---
;; Performance: For very large agenda files, consider `(setq org-agenda-inhibit-startup t)`
;; to prevent slowdowns from file startup options.
(after! org-agenda
  (setq org-agenda-skip-scheduled-if-done t
        org-agenda-skip-deadline-if-done t
        org-agenda-include-deadlines t
        org-agenda-block-separator 'hr
        org-agenda-compact-blocks t
        org-agenda-start-with-log-mode t)

  (setq org-agenda-custom-commands
        '(("d" "📅 Dashboard"
           ((agenda "" ((org-deadline-warning-days 7)
                        (org-agenda-overriding-header "📅 Agenda")))
            (todo "⚡ NEXT" ((org-agenda-overriding-header "⚡ Next Tasks")))
            (tags-todo "project/🚀 ACTIVE" ((org-agenda-overriding-header "🚀 Active Projects")))
            (tags-todo "+PRIORITY=\"A\"" ((org-agenda-overriding-header "🔥 High Priority")))
            (todo "⏳ WAIT" ((org-agenda-overriding-header "⏳ Waiting On")))
            (tags-todo "+habit" ((org-agenda-overriding-header "🔄 Habits")))
            ;; Performance Warning: The "stuck" agenda can be slow.
            ;; Consider defining what a stuck project is for you via `org-stuck-projects`.
            (stuck "" ((org-agenda-overriding-header "🚫 Stuck Projects")))))
          ("p" "📋 Projects Overview"
           ((tags "project" ((org-agenda-overriding-header "📋 All Projects")))))
          ("g" "🎯 Goals Review"
           ((tags-todo "goal" ((org-agenda-overriding-header "🎯 Goals"))))))))

(use-package! org-super-agenda
  :hook (org-agenda-mode . org-super-agenda-mode)
  :config
  ;; The order of these groups is important for filtering.
  (setq org-super-agenda-groups
        '((:name "🔥 Overdue" :deadline past)
          (:name "📅 Today" :time-grid t :scheduled today)
          (:name "⚡ Next" :todo "⚡ NEXT")
          (:name "🔥 Important" :priority "A")
          (:name "🚀 Active Projects" :tag "project" :todo "ACTIVE")
          (:name "🎯 Goals" :tag "goal")
          (:name "🔄 Habits" :tag "habit")
          (:name "⏳ Waiting" :todo "WAIT")
          (:discard (:anything t)))))
```
