## config.el

```el

(defvar my/org-directory "~/org/" "Base directory for all org files.")
(defvar my/org-roam-directory (expand-file-name "roam/" my/org-directory) "Directory for org-roam files.")

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

(defun ar/org-font-setup ()
  (dolist (face '((org-level-1 . 1.2) (org-level-2 . 1.1) (org-level-3 . 1.05)
                  (org-level-4 . 1.0) (org-level-5 . 1.1) (org-level-6 . 1.1)
                  (org-level-7 . 1.1) (org-level-8 . 1.1)))
    (set-face-attribute (car face) nil :font "JetBrainsMono Nerd Font" :weight 'bold :height (cdr face) :slant 'unspecified))
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
  (ar/org-font-setup))

(after! org
  (setq org-directory my/org-directory
        org-agenda-files '("~/org/inbox.org" "~/org/projects.org" "~/org/habits.org" "~/org/goals.org")
        org-default-notes-file (expand-file-name "inbox.org" my/org-directory)
        org-startup-with-inline-images t
        org-image-actual-width 600
        org-archive-location (concat (file-name-as-directory (expand-file-name "archive" my/org-directory)) "Archive_%s::")
        org-auto-align-tags nil
        org-hide-emphasis-markers t)
  (add-hook! org-mode #'ar/org-setup-hook))

(use-package! org-tempo
  :after org
  :config
  (add-to-list 'org-structure-template-alist '("sh" . "src shell"))
  (add-to-list 'org-structure-template-alist '("py" . "src python"))
  (add-to-list 'org-structure-template-alist '("el" . "src emacs-lisp")))

(setf (alist-get 'height +org-capture-frame-parameters) 15)

(after! org
  (setq org-todo-keywords
        '((sequence "📥 TODO(t)" "⚡ NEXT(n)" "⚙️ PROG(p)" "⏳ WAIT(w@/!)" "|" "✅ DONE(d!)" "❌ CANCEL(c@)")
          (sequence "📝 PLAN(P)" "🚀 ACTIVE(A)" "⏸️ PAUSED(x)" "|" "🏆 ACHIEVED(a)" "🗑️ DROPPED(D)")))
  (setq org-todo-keyword-faces
        `(("📥 TODO" . (:foreground ,(doom-color 'red) :weight bold))
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

(use-package! org-fragtog :hook (org-mode . org-fragtog-mode))

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
  :commands org-roam-ui-mode3
  :config
  (setq org-roam-ui-sync-theme t
        org-roam-ui-follow t
        org-roam-ui-update-on-save t
        org-roam-ui-open-on-start nil))

(use-package! consult-org-roam
  :after (consult org-roam)
  :config (consult-org-roam-mode 1))

(use-package! org-super-agenda
  :hook (org-agenda-mode . org-super-agenda-mode)
  :config
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
            (stuck "" ((org-agenda-overriding-header "🚫 Stuck Projects")))))
          ("p" "📋 Projects Overview"
           ((tags "project" ((org-agenda-overriding-header "📋 All Projects")))))
          ("g" "🎯 Goals Review"
           ((tags-todo "goal" ((org-agenda-overriding-header "🎯 Goals"))))))))

```

## init.el

```init.el
;;; init.el -*- lexical-binding: t; -*-

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
       ;; (emoji +unicode)    ; 🙂
       hl-todo           ; highlight TODO/FIXME/NOTE/DEPRECATED/HACK/REVIEW
       indent-guides       ; highlighted indent columns
       (ligatures +extra)
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
       (treemacs +lsp)
       ;;unicode           ; extended unicode support for various languages
       (vc-gutter +pretty) ; vcs diff in the fringe
       ;;vi-tilde-fringe   ; fringe tildes to mark beyond EOB
       ;;window-select       ; visually switch windows
       workspaces          ; tab emulation, persistence & separate workspaces
       ;;zen                 ; distraction-free coding or writing

       :editor
       (evil +everywhere)  ; come to the dark side, we have cookies
       file-templates      ; auto-snippets for empty files
       fold                ; (nigh) universal code folding
       (format
        +onsave)
       ;;god               ; run Emacs commands without modifier keys
       ;;lispy             ; vim for lisp, for people who don't like vim
       ;;multiple-cursors  ; editing in many places at once
       ;;objed             ; text object editing for the innocent
       ;;parinfer          ; turn lisp into python, sort of
       rotate-text       ; cycle region at point between text candidates
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
        +icons)
       ;;grammar           ; tasing grammar mistake every you make

       :tools
       ;;ansible
       biblio              ; Writes a PhD for you (citation needed)
       ;;collab            ; buffers with friends
       debugger
       direnv
       ;;docker
       ;;editorconfig      ; let someone else argue about tabs vs spaces
       ;;ein               ; tame Jupyter notebooks with emacs
       (eval +overlay)     ; run code, run (also, repls)
       lookup             ; navigate your code and its documentation
       ;;llm               ; when I said you needed friends, I didn't mean...
       (lsp
         +peek)
       (magit
        +forge)
       ;;make              ; run make tasks from Emacs
       ;;pass              ; password manager for nerds
       pdf                 ; pdf enhancements
       ;;terraform         ; infrastructure as code
       ;;tmux                ; an API for interacting with tmux
       tree-sitter         ; syntax and parsing, sitting in a tree...
       ;;upload            ; map local to remote projects via ssh/ftp

       :os
       ;;(tty +osc)

       :lang
       ;;agda              ; types of types of types of types...
       ;;beancount         ; mind the GAAP
       (cc
        +tree-sitter
        +lsp)
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
       (markdown
        +tree-sitter
        +grip)
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
        +pyright
        +tree-sitter
        +lsp)
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
        +lsp)
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

## packages.el

```el
;; --- Theme & UI ---
(package! rainbow-delimiters)
(package! buffer-terminator)

;; --- Editor Behaviour & Completion ---
(package! helpful)
(package! wgrep)
;;(package! jinx)
(package! consult-yasnippet)
(package! yasnippet-capf)
                                        ;
;; --- Evil ---
(package! evil-multiedit)

;; --- Org & Roam ---
(package! org-roam-ui)
(package! org-super-agenda)
(package! consult-org-roam)
(package! org-fragtog)

;; --- Citations & LaTeX ---
(package! citar-org-roam)
(package! citar-embark)
(package! laas)
(package! math-symbol-lists)
;;(package! ox-chameleon :recipe (:host github :repo "tecosaur/ox-chameleon"))
;;(package! engrave-faces :recipe (:host github :repo "tecosaur/engrave-faces"))

;; --- Development & System ---
(package! feature-mode)
(package! dired-open)
(package! dired-git-info)
(package! dired-ranger)

;; --- Ignored Packages ---
(package! helm-bibtex :ignore t)
;; (package! hydra :ignore t)
```
