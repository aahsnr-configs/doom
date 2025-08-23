```el
;; -*- no-byte-compile: t; -*-
;;; $DOOMDIR/packages.el

;; To install a package with Doom you must declare them here and run 'doom sync'
;; on the command line, then restart Emacs for the changes to take effect -- or


;; To install SOME-PACKAGE from MELPA, ELPA or emacsmirror:
;; (package! some-package)

;; To install a package directly from a remote git repo, you must specify a
;; `:recipe'. You'll find documentation on what `:recipe' accepts here:
;; https://github.com/radian-software/straight.el#the-recipe-format
;; (package! another-package
;;   :recipe (:host github :repo "username/repo"))

;; If the package you are trying to install does not contain a PACKAGENAME.el
;; file, or is located in a subdirectory of the repo, you'll need to specify
;; `:files' in the `:recipe':
;; (package! this-package
;;   :recipe (:host github :repo "username/repo"
;;            :files ("some-file.el" "src/lisp/*.el")))

;; If you'd like to disable a package included with Doom, you can do so here
;; with the `:disable' property:
;; (package! builtin-package :disable t)

;; You can override the recipe of a built in package without having to specify
;; all the properties for `:recipe'. These will inherit the rest of its recipe
;; from Doom or MELPA/ELPA/Emacsmirror:
;; (package! builtin-package :recipe (:nonrecursive t))
;; (package! builtin-package-2 :recipe (:repo "myfork/package"))

;; Specify a `:branch' to install a package from a particular branch or tag.
;; This is required for some packages whose default branch isn't 'master' (which
;; our package manager can't deal with; see radian-software/straight.el#279)
;; (package! builtin-package :recipe (:branch "develop"))

;; Use `:pin' to specify a particular commit to install.
;; (package! builtin-package :pin "1a2b3c4d5e")


;; Doom's packages are pinned to a specific commit and updated from release to
;; release. The `unpin!' macro allows you to unpin single packages...
;; (unpin! pinned-package)
;; ...or multiple packages
;; (unpin! pinned-package another-pinned-package)
;; ...Or *all* packages (NOT RECOMMENDED; will likely break things)
;; (unpin! t)
;;; -*- lexical-binding: t; -*-

;;; -*- lexical-binding: t; -*-

;; Performance
(package! so-long)

;; UI & Theming
(package! rainbow-delimiters)
(package! nerd-icons-completion)
(package! nerd-icons-corfu)
(package! nerd-icons-dired)
(package! nerd-icons-ibuffer)
(package! indent-bars)
(package! info-colors)

;; Editor Behavior & Workflow
(package! shackle)
(package! helpful)
(package! wgrep)
(package! buffer-terminator)

;; Evil Extensions
(package! evil-lion)
(package! evil-multiedit)
(package! evil-goggles)
(package! evil-numbers)
(package! evil-args)
(package! evil-anzu)
(package! evil-exchange)
(package! evil-indent-plus)
(package! evil-visualstar)
(package! evil-matchit)
(package! evil-snipe)

;; Completion & Snippets
(package! consult-yasnippet)
(package! yasnippet-snippets)
(package! yasnippet-capf)

;; Org & Roam
(package! org-roam-ui)
(package! doct)
(package! org-super-agenda)
(package! org-modern)
(package! org-appear)
(package! consult-org-roam)

;; Version Control
(package! magit-todos)
(package! git-timemachine)

;; LaTeX & Citations
(package! laas)
(package! citar-org-roam)
(package! citar-embark)

;; Dired
(package! dired-open)
(package! dired-ranger)
(package! fd-dired)

;; Ignored Packages
(package! hydra :ignore t)
(package! helm-bibtex :ignore t)
(package! flycheck-popup-tip :ignore t)
(package! flycheck-posframe :ignore t)

```
