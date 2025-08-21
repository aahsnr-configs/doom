;;; init.el -*- lexical-binding: t; -*-

(doom! :input
       ;;layout

       :completion
       (vertico
        +icons)
       (corfu
        +icons
        +orderless
        +dabbrev)

       :ui
       doom
       doom-dashboard
       (emoji
        +unicode)
       hl-todo
       indent-guides
       (ligatures
        +extra)
       modeline
       ophints
       (popup +all)
       (smooth-scroll
        +interpolate)
       treemacs
       (vc-gutter
        +pretty)
       window-select
       workspaces
       zen

       :editor
       (evil +everywhere)
       file-templates
       fold
       (format +onsave)
       snippets
       word-wrap

       :emacs
       (dired
        +dirvish
        +icons)
       electric
       (ibuffer
        +icons)
       undo
       vc

       :term
       vterm

       :checkers
       (syntax
        +flymake
        +icons)
       (spell
        +enhant)

       :tools
       biblio
       debugger
       direnv
       (eval +overlay)
       lookup
       (lsp +eglot)
       (magit +forge)
       pdf
       tree-sitter

       :lang
       emacs-lisp
       (latex
        +cdlatex
        +fold
        +lsp)
       markdown
       (nix
        +tree-sitter
        +lsp)
       (org
        +dragndrop
        +gnuplot
        +jupyter
        +noter
        +pandoc
        +pretty
        +roam2)
       (python
        +lsp
        +tree-sitter)
       (sh
        +lsp
        +tree-sitter)

       :config
       ;;literate
       (default +bindings +smartparens))
