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
(package! hydra :disable t)
(package! helm-bibtex :disable t)
(package! helm-bibtex :disable t)

(package! flymake-posframe :ignore t)
(package! hydra :ignore t)
(package! helm-bibtex :ignore t)

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

;; UI & Theme Enhancements
(package! mixed-pitch)
(package! visual-fill-column)
(package! info-colors)
(package! nerd-icons-dired)

;; Evil Enhancements
(package! evil-goggles)
(package! evil-lion)
(package! evil-textobj-tree-sitter)
(package! string-inflection)

;; Org Mode & Note Taking
(package! org-roam-ui)
(package! doct)
(package! org-super-agenda)
(package! org-fragtog)
;; Completion & Searching
(package! consult-yasnippet)
(package! consult-org-roam)

;; LaTeX & Citations
(package! citar-embark)
(package! laas)

;; Development
(package! flymake-collection)
(package! sideline-flymake)
(package! eldoc-box)
(package! vlf)  ;tecosaur config

;; Markdown
(package! vmd-mode)

;; Pandoc Integration
(package! pandoc-mode)


