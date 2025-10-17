;;; init.el -*- lexical-binding: t; -*-

(doom! :input
       ;;bidi              ; (tfel ot) thgir etirw uoy gnipleh
       ;;chinese
       ;;japanese
       ;;layout            ; auie,ctsrnm is the superior home row

       :completion
       (corfu +icons +orderless +dabbrev)
       (vertico +icons)

       :ui
       doom
       doom-dashboard
       hl-todo
       indent-guides
       (ligatures +extra)
       minimap
       modeline
       ophints
       (popup +all +defaults)
       ;;(smooth-scroll +interpolate)
       treemacs
       (vc-gutter +pretty)
       workspaces          
       :editor
       (evil +everywhere)
       file-templates
       fold
       (format +onsave)
       rotate-text
       snippets
       word-wrap

       :emacs
       (dired +dirvish +icons)
       electric
       (ibuffer +icons)
       undo
       vc

       :term
       vterm

       :checkers
       (syntax +flymake +icons)

       :tools
       biblio
       debugger
       direnv
       (eval +overlay)
       lookup
       (lsp +eglot +booster)
       (magit +forge)
       pdf
       tree-sitter

       :os
       ;;(tty +osc)

       :lang
       (cc +lsp)
       emacs-lisp
       (latex +cdlatex +fold +lsp)
       (markdown +grip)
       (nix +lsp)
       (org
        +dragndrop
        +gnuplot
        +jupyter
        +noter
        +pandoc
        +pretty
        +present
        +roam)
       (python +lsp +tree-sitter)
       (sh +fish +lsp)

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
       ;;literate
       (default +bindings +smartparens))
