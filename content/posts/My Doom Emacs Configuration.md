+++
title = "My Doom Emacs Configuration"
author = ["Eejain Huang"]
date = 2020-09-08
tags = ["technology"]
categories = ["Meta"]
draft = true
weight = 1013
bookComments = true
bookHidden = true
bookToC = true
+++

{{< figure src="~/itflows/static/doomsnap.png" caption="Figure 1: Emacs main window in iTerm2" >}}

{{< figure src="~/itflows/static/doomsnap2.png" caption="Figure 2: Emacs quickshow window with Org agenda" >}}


## Intro {#intro}


### Forewords {#forewords}

-   A few words about my background. I'm a researcher in the field of educational science. I am no programmer, other than borrowing codes from stack overflow all the time. My experience with plain text editors started with writing monkey, and then Sublime Text. But Emacs quickly replaced almost every app in both my work and leisure workflow. I'm currently using Emacs for almost all the tasks: develop R script, interacting with shell, composing long-format texts, publish personal blog, and manage my schedule as well as logging everyday journal.
-   I'm using [GNU Emacs](https://formulae.brew.sh/formula/emacs) under the [Doom Emacs](https://github.com/hlissner/doom-emacs) configuration framework on macOS.
    -   I usually have two instances of Emacs running: terminal frame and Emacs GUI application.
    -   First approach with terminal frame: I'm running my Emacs in[ iTerm2](https://www.iterm2.com/)--a terminal emulator for macOS--with zsh. Serve as quick-show window for org-agenda, or for direct interaction with kernel.
    -   Second approach with GUI: Emacs-mac port, able to handle picture and pdf, and has smoother interface than Emacs-plus. Serve as the main editor.


### Instructions {#instructions}


#### Installation {#installation}

-   To install GNU Emacs and Doom, run the following in terminal. References: <https://github.com/hlissner/doom-emacs/blob/develop/docs/getting%5Fstarted.org#with-homebrew>

<!--listend-->

```text

brew install git ripgrep
brew install coreutils fd
xcode-select --install

brew tap railwaycat/emacsmacport
brew install emacs-mac --with-modules
ln -s /usr/local/opt/emacs-mac/Emacs.app /Applications/Emacs.app

git clone --depth 1 https://github.com/hlissner/doom-emacs ~/.emacs.d
~/.emacs.d/bin/doom install

```


#### Doom utility usage {#doom-utility-usage}

-   Add `export PATH=~/.emacs.d/bin:$PATH` in <span class="underline">.zshrc</span> to use the bin/doom utility anywhere in terminal.
-   Turn on `literate` in <span class="underline">init.el</span> in <span class="underline">~/.dooms.d</span>, then create <span class="underline">config.org</span> in Emacs.
-   Noting that the private config files are not stored in <span class="underline">~/.emacs.d</span>.
-   Add `#+PROPERTY: header-args :tangle yes :cache yes :results silent :padline no` anywhere in the <span class="underline">config.org</span>.
-   Run `M-x doom/reload` in Emacs after changing the <span class="underline">config.org</span>, this will extract the Lisp source codes automatically to <span class="underline">config.el</span>.
-   Run `doom sync` in terminal every time the contents in <span class="underline">package.el</span> has been changed.


#### Debug workflow {#debug-workflow}

-   turn on `toggle-debug-on-error`, then `doom/reload`
-   run `doom doctor` in terminal
-   run `doom build` to recompile all installed packages (<https://github.com/hlissner/doom-emacs/blob/develop/docs/getting%5Fstarted.org#update--rollback>)
-   reinstall all packages by deleting <span class="underline">~/.emacs.d/.local</span>, then run `doom sync`.
-   reinstall doom if the issues still sustain
    -   backup the current dot files in <span class="underline">.doom.d</span>
    -   remove <span class="underline">.doom.d</span> and <span class="underline">.emacs.d</span> from the home directory
    -   reinstall doom emacs (<https://github.com/hlissner/doom-emacs/blob/develop/docs/getting%5Fstarted.org#doom-emacs>) [wait]
    -   turn on literate in <span class="underline">inti.el</span>
    -   replace <span class="underline">config.org</span>, <span class="underline">packages.el</span> with own dot files, run `doom sync` [wait]
    -   change <span class="underline">init.el</span> settings manually


## Doom Modules/init.el {#doom-modules-init-dot-el}

-   exports to init.el under DOOMDIR
-   <https://github.com/hlissner/doom-emacs/blob/develop/docs/modules.org>

<!--listend-->

<a id="code-snippet--init.el"></a>
```emacs-lisp
;;; init.el -*- lexical-binding: t; -*-

(doom! :input
       ;; chinese
       ;; japanese
       ;; layout            ; auie,ctsrnm is the superior home row

       :completion
       ;; company           ; the ultimate code completion backend
       (company
        +auto
        +childframe)
       helm              ; the *other* search engine for love and life
       ;; ido               ; the other *other* search engine...
       ;; ivy               ; a search engine for love and life
       (ivy
       +icons
       +prescient
       +fuzzy
       +childframe)

       :ui
       ;; deft              ; notational velocity for Emacs
       ;; doom              ; what makes DOOM look the way it does
       ;; doom-dashboard    ; a nifty splash screen for Emacs
       ;; doom-quit         ; DOOM quit-message prompts when you quit Emacs
       (emoji +unicode)  ; 🙂
       fill-column       ; a `fill-column' indicator
       ;; hl-todo           ; highlight TODO/FIXME/NOTE/DEPRECATED/HACK/REVIEW
       hydra
       ;; indent-guides     ; highlighted indent columns
       (ligatures +extra)         ; ligatures and symbols to make your code pretty again
       minimap           ; show a map of the code on the side
       ;; modeline          ; snazzy, Atom-inspired modeline, plus API
       ;; nav-flash         ; blink cursor line after big motions
       neotree           ; a project drawer, like NERDTree for vim
       ophints           ; highlight the region an operation acts on
       (popup +all +defaults)   ; tame sudden yet inevitable temporary windows
       ;; tabs              ; a tab bar for Emacs
       ;; treemacs          ; a project drawer, like neotree but cooler
       unicode           ; extended unicode support for various languages
       ;; vc-gutter         ; vcs diff in the fringe
       vi-tilde-fringe   ; fringe tildes to mark beyond EOB
       window-select     ; visually switch windows
       workspaces        ; tab emulation, persistence & separate workspaces
       zen               ; distraction-free coding or writing

       :editor
       (evil +everywhere); come to the dark side, we have cookies
       file-templates    ; auto-snippets for empty files
       fold              ; (nigh) universal code folding
       (format +onsave)  ; automated prettiness
       ;; god               ; run Emacs commands without modifier keys
       ;; lispy             ; vim for lisp, for people who don't like vim
       multiple-cursors  ; editing in many places at once
       ;; objed             ; text object editing for the innocent
       ;; parinfer          ; turn lisp into python, sort of
       ;; rotate-text       ; cycle region at point between text candidates
       snippets          ; my elves. They type so I don't have to
       word-wrap         ; soft wrapping with language-aware indent

       :emacs
       (dired +icons)             ; making dired pretty [functional]
       electric          ; smarter, keyword-based electric-indent
       (ibuffer +icons)         ; interactive buffer management
       ;; undo              ; persistent, smarter undo for your inevitable mistakes
       vc                ; version-control and Emacs, sitting in a tree

       :term
       ;; eshell            ; the elisp shell that works everywhere
       ;; shell             ; simple shell REPL for Emacs
       ;; term              ; basic terminal emulator for Emacs
       ;; vterm             ; the best terminal emulation in Emacs

       :checkers
       syntax              ; tasing you for every semicolon you forget
       (spell +flyspell) ; tasing you for misspelling mispelling
       ;; grammar           ; tasing grammar mistake every you make

       :tools
       ;; ansible
       ;; debugger          ; FIXME stepping through code, to help you add bugs
       ;; direnv
       ;; docker
       ;; editorconfig      ; let someone else argue about tabs vs spaces
       ;; ein               ; tame Jupyter notebooks with emacs
       (eval +overlay)     ; run code, run (also, repls)
       ;; gist              ; interacting with github gists
       (lookup
        +dictionary
        +docsets)              ; navigate your code and its documentation
       ;; (lsp +peek)
       (magit +forge)             ; a git porcelain for Emacs
       ;; make              ; run make tasks from Emacs
       ;; pass              ; password manager for nerds
       ;; pdf               ; pdf enhancements
       ;; prodigy           ; FIXME managing external services & code builders
       ;; rgb               ; creating color strings
       ;; taskrunner        ; taskrunner for all your projects
       ;; terraform         ; infrastructure as code
       ;; tmux              ; an API for interacting with tmux
       ;; upload            ; map local to remote projects via ssh/ftp

       :os
       (:if IS-MAC macos)  ; improve compatibility with macOS
       tty               ; improve the terminal Emacs experience

       :lang
       ;; agda              ; types of types of types of types...
       ;; beancount         ; mind the GAAP
       ;; cc                ; C > C++ == 1
       ;; clojure           ; java with a lisp
       ;; common-lisp       ; if you've seen one lisp, you've seen them all
       ;; coq               ; proofs-as-programs
       ;; crystal           ; ruby at the speed of c
       ;; csharp            ; unity, .NET, and mono shenanigans
       ;; data              ; config/data formats
       ;; (dart +flutter)   ; paint ui and not much else
       ;; elixir            ; erlang done right
       ;; elm               ; care for a cup of TEA?
       emacs-lisp        ; drown in parentheses
       ;; erlang            ; an elegant language for a more civilized age
       ess               ; emacs speaks statistics
       ;; factor
       ;; faust             ; dsp, but you get to keep your soul
       ;; fsharp            ; ML stands for Microsoft's Language
       ;; fstar             ; (dependent) types and (monadic) effects and Z3
       ;; gdscript          ; the language you waited for
       ;; (go +lsp)         ; the hipster dialect
       ;; (haskell +dante)  ; a language that's lazier than I am
       ;; hy                ; readability of scheme w/ speed of python
       ;; idris             ; a language you can depend on
       ;; json              ; At least it ain't XML
       ;; (java +meghanada) ; the poster child for carpal tunnel syndrome
       ;; javascript        ; all(hope(abandon(ye(who(enter(here))))))
       ;; julia             ; a better, faster MATLAB
       ;; kotlin            ; a better, slicker Java(Script)
       (latex
        +latexmk
        +cdlatex
        +fold)             ; writing papers in Emacs has never been so fun
       ;; lean              ; for folks with too much to prove
       ;; ledger            ; be audit you can be
       ;; lua               ; one-based indices? one-based indices
       markdown          ; writing docs for people to ignore
       ;; nim               ; python + lisp at the speed of c
       ;; nix               ; I hereby declare "nix geht mehr!"
       ;; ocaml             ; an objective camel
       (org
        +attach
        +babel
        +capture
        +dragndrop
        +hugo
        +export
        +pandoc
        +gnuplot
        +pretty
        +present
        +protocol
        +pomodoro
        +roam2
        +noter
       )               ; organize your plain life in plain text
       ;; php               ; perl's insecure younger brother
       ;  plantuml          ; diagrams for confusing people more
       ;; purescript        ; javascript, but functional
       ;; python            ; beautiful is better than ugly
       ;; qt                ; the 'cutest' gui framework ever
       ;; racket            ; a DSL for DSLs
       ;; raku              ; the artist formerly known as perl6
       ;; rest              ; Emacs as a REST client
       ;; rst               ; ReST in peace
       ;; (ruby +rails)     ; 1.step {|i| p "Ruby is #{i.even? ? 'love' : 'life'}"}
       ;; rust              ; Fe2O3.unwrap().unwrap().unwrap().unwrap()
       ;; scala             ; java, but good
       ;; (scheme +guile)   ; a fully conniving family of lisps
       ;; sh                ; she sells {ba,z,fi}sh shells on the C xor
       ;; sml
       ;; solidity          ; do you need a blockchain? No.
       ;; swift             ; who asked for emoji variables?
       ;; terra             ; Earth and Moon in alignment for performance.
       ;; web               ; the tubes
       yaml              ; JSON, but readable
       ;; zig               ; C, but simpler

       :email
       ;; (mu4e +gmail)
       ;; notmuch
       ;; (wanderlust +gmail)

       :app
       ;; calendar
       ;; emms
       ;; everywhere        ; *leave* Emacs!? You must be joking
       ;; irc               ; how neckbeards socialize
       ;; (rss +org)        ; emacs as an RSS reader
       ;; twitter           ; twitter client https://twitter.com/vnought

       :config
       literate
       (default +bindings +smartparens))

(setq evil-respect-visual-line-mode t)
```


## Package Preparation {#package-preparation}

exports to packages.el in DOOMDIR

```emacs-lisp

;; macOS specific
(package! osx-clipboard
  :recipe (:host github :repo "joddie/osx-clipboard-mode"))

;; look and interface
(package! centered-window
  :recipe (:host github :repo "anler/centered-window-mode"))
(package! adaptive-wrap)
(package! posframe)
(package! sublimity)
(package! sr-speedbar)
(package! swiper)
(package! multiple-cursors)
(package! smex)
(package! popper
  :recipe (:host github :repo "waymondo/popper"))
(package! dired-k)
(package! gitconfig-mode
    :recipe (:host github :repo "magit/git-modes"
       :files ("gitconfig-mode.el")))
(package! gitignore-mode
    :recipe (:host github :repo "magit/git-modes"
       :files ("gitignore-mode.el")))

;; org mode
(package! org-journal)
(package! ox-hugo)
(package! toc-org)
(package! helm-org)
(package! helm-org-rifle)
(package! org-super-agenda)
;; (package! org-agenda-property)
;; (package! org-preview-html)
(package! calfw)
(package! calfw-ical)
(package! calfw-org)
(package! origami)
(package! org-pandoc-import
  :recipe (:host github
           :repo "tecosaur/org-pandoc-import"
           :files ("*.el" "filters" "preprocessors")))
(package! ox-pandoc)
(package! org-ql)
(package! helm-org-ql)
(package! org-ol-tree
  :recipe (:host github :repo "Townk/org-ol-tree"))
(package! org-sidebar)
(package! org-roam)
(package! org-super-links
  :recipe (:host github :repo "toshism/org-super-links"))

;; language
(package! pyim)
(package! pyim-basedict)
(package! parrot)

;; statistics
(package! ess)
(package! polymode)
(package! poly-markdown)
(package! poly-R)
(package! poly-org)

;; references
(package! zotxt)
(package! pdf-tools)
(package! org-noter)

;; feed
(package! elfeed)

;; misc
(package! psession)
;; (package! simpleclip)
;; (package! cliphist)
;; (package! play-sound
;;   :recipe (:host github :repo "leoliu/play-sound-osx"))
; (disable-packages! bookmark tide eldoc valign grip-mode pyim)

;; doom default
;; completion/company
(package! prescient)
(package! company-prescient)

;; completion/ivy
(package! all-the-icons-ivy)
(package! counsel-tramp)

;; emacs/dired
(package! peep-dired)
(package! diredfl)

;; feature/snippets
(package! yasnippet-snippets)

;; lang/apache
(package! apache-mode)

;; lang/pkgbuild
(package! pkgbuild-mode)

;; lang/nginx
(package! nginx-mode)

;; lang/org
(package! ob-http)

;; lang/systemd
(package! systemd)

;; lang/sh
(package! flycheck-checkbashisms)

;; tools/tldr
;; (package! tldr)

;; ui/doom
; (package! doom-themes :recipe (:fetcher github :repo "brettm12345/emacs-doom-themes" :files ("*.el" "themes/*.el")))
;; (package! doom-palenight-theme :recipe (:fetcher github :repo "brettm12345/doom-palenight-theme"))

;; ui/modeline
;; (package! doom-modeline)
(package! anzu)
(package! evil-anzu)

;; ui/indent-guides
; (package! highlight-indent-guides)

```


## Global Backends {#global-backends}


### Front matter {#front-matter}

<https://www.cheng92.com/emacs/doom-emacs-with-org/>

```emacs-lisp

(package-initialize t)
;;(doom-load-envvars-file "~/.doom.d/env" )
(add-to-list 'initial-frame-alist '(fullscreen . maximized))
(setq user-full-name "Eejain Huang"
      user-mail-address "huangyizhen2002@gmail.com")
```


### Set encoding and language environment {#set-encoding-and-language-environment}

```emacs-lisp

(prefer-coding-system       'utf-8)
(set-default-coding-systems 'utf-8)
(set-terminal-coding-system 'utf-8)
(set-keyboard-coding-system 'utf-8)
(set-language-environment 'utf-8)
(setq mm-coding-system-priorities '(utf-8))
(setq default-buffer-file-coding-system 'utf-8)
(setq ispell-dictionary "en")

```


### Do not create lockfiles for files being edited {#do-not-create-lockfiles-for-files-being-edited}

references: <https://github.com/Brettm12345/doom-emacs-literate-config/blob/master/config.org#completioncompany>

```emacs-lisp
  (setq create-lockfiles nil)
```


### Parentheses match {#parentheses-match}

```emacs-lisp

(defun my-goto-match-paren (arg)
  "Go to the matching if on (){}[], similar to vi style of % ."
  (interactive "p")
  (cond ((looking-at "[\[\(\{]") (evil-jump-item))
        ((looking-back "[\]\)\}]" 1) (evil-jump-item))
        ((looking-at "[\]\)\}]") (forward-char) (evil-jump-item))
        ((looking-back "[\[\(\{]" 1) (backward-char) (evil-jump-item))
        (t nil)))

(map!
(:after evil
  :m "M--" #'my-goto-match-paren))

(sp-local-pair
 '(org-mode)
 "<<" ">>"
 :actions '(insert))

(use-package smartparens
  :init
  (map! :map smartparens-mode-map
  (:after evil
     :m "C-)" #'sp-forward-slurp-sexp
     :m "C-(" #'sp-forward-barf-sexp
     :m "C-{" #'sp-backward-slurp-sexp
     :m "C-}" #'sp-backward-barf-sexp
       )))
```


### Dired config {#dired-config}

reference: <https://github.com/Brettm12345/doom-emacs-literate-config/blob/master/config.org#completioncompany>


#### Set `dired-k` to use human readable styles {#set-dired-k-to-use-human-readable-styles}

```emacs-lisp

  (after! dired-k
    (setq dired-k-human-readable t))
  (after! dired-k
    (setq dired-k-size-colors
          `((1024 .   ,(doom-lighten (doom-color 'green) 0.3))
            (2048 .   ,(doom-lighten (doom-color 'green) 0.2))
            (3072 .   ,(doom-lighten (doom-color 'green) 0.1))
            (5120 .   ,(doom-color 'green))
            (10240 .  ,(doom-lighten (doom-color 'yellow) 0.2))
            (20480 .  ,(doom-lighten (doom-color 'yellow) 0.1))
            (40960 .  ,(doom-color 'yellow))
            (102400 . ,(doom-lighten (doom-color 'orange) 0.2))
            (262144 . ,(doom-lighten (doom-color 'orange) 0.1))
            (524288 . ,(doom-color 'orange)))))
```


#### Enable `diredfl-mode` on `dired` buffers {#enable-diredfl-mode-on-dired-buffers}

```emacs-lisp

  (use-package diredfl
    :hook (dired-mode . diredfl-mode))

```


#### Setup `peep-dired` {#setup-peep-dired}

```emacs-lisp

  (use-package peep-dired
    :after dired
    :defer t
    :commands (peep-dired))

```


### Search functionalities {#search-functionalities}

ref:

-   use helm-org-rifle for org file search
-   use swiper instead of I-search for in buffer search <http://pragmaticemacs.com/emacs/dont-search-swipe/>
-   <https://github.com/novoid/dot-emacs/blob/master/config.org>

<!--listend-->

```emacs-lisp

(use-package swiper
  :config
  (setq ivy-display-style 'fancy) ;; fancy highlighting

  ;;advise swiper to recenter on exit
  (defun bjm-swiper-recenter (&rest args)
    "recenter display after swiper"
    (recenter)
    )

  (global-set-key (kbd "C-s") 'swiper)
)

(use-package helm
  :defer 110
)

(use-package helm-org
  :defer 90
  :config
  (add-to-list 'helm-completing-read-handlers-alist '(org-capture . helm-org-completing-read-tags))
  (add-to-list 'helm-completing-read-handlers-alist '(org-set-tags . helm-org-completing-read-tags))
)

(use-package helm-org-rifle
  :defer 110
  :after org
)

```


### Multiple cursor {#multiple-cursor}

ref: <https://github.com/magnars/multiple-cursors.el>

```emacs-lisp
(require 'multiple-cursors)
(global-set-key (kbd "C-S-c C-S-c") 'mc/edit-lines)
(global-set-key (kbd "C->") 'mc/mark-next-like-this)
(global-set-key (kbd "C-<") 'mc/mark-previous-like-this)
(global-set-key (kbd "C-c C-<") 'mc/mark-all-like-this)
```


### Pop-up window management {#pop-up-window-management}

```emacs-lisp
(use-package popper
  :bind
  ("C-`" . popper-toggle-latest)
  ("C-~" . popper-cycle)
  ("C-s-`" . popper-kill-latest-popup)
  :custom
  (popper-reference-buffers
   '("*eshell*"
     "*vterm*"
     "Output\\*$"
     "*Process List*"
     "COMMIT_EDITMSG"
     "*Warnings*"
     embark-collect-mode
     deadgrep-mode
     grep-mode
     rg-mode
     rspec-compilation-mode
     inf-ruby-mode
     nodejs-repl-mode
     ts-comint-mode
     compilation-mode))
  :config
  (defun zero-point-thirty-seven () 0.37)
  (advice-add 'popper-determine-window-height :override #'zero-point-thirty-seven)
  :init
  (popper-mode)
  )
```


## Global Looks {#global-looks}


### Theme {#theme}

-   current theme: <https://github.com/rougier/nano-emacs>
-   doom default: <https://github.com/hlissner/emacs-doom-themes/tree/screenshots>

<!--listend-->

```emacs-lisp
;; add private pacakge: https://github.com/hlissner/doom-emacs/issues/1213

(add-load-path! "lisp")

;; Default layout (optional)
;; (require 'nano-layout)

;; Theming Command line options (this will cancel warning messages)
(add-to-list 'command-switch-alist '("-dark"   . (lambda (args))))
(add-to-list 'command-switch-alist '("-light"  . (lambda (args))))
(add-to-list 'command-switch-alist '("-default"  . (lambda (args))))
(add-to-list 'command-switch-alist '("-no-splash" . (lambda (args))))
(add-to-list 'command-switch-alist '("-no-help" . (lambda (args))))
(add-to-list 'command-switch-alist '("-compact" . (lambda (args))))

(cond
 ((member "-default" command-line-args) t)
 ((member "-dark" command-line-args) (require 'nano-theme-dark))
 (t (require 'nano-theme-light)))

;; Customize support for 'emacs -q' (Optional)
;; You can enable customizations by creating the nano-custom.el file
;; with e.g. `touch nano-custom.el` in the folder containing this file.
;; (let* ((this-file  (or load-file-name (buffer-file-name)))
;;        (this-dir  (file-name-directory this-file))
;;        (custom-path  (concat this-dir "nano-custom.el")))
;;   (when (and (eq nil user-init-file)
;;              (eq nil custom-file)
;;              (file-exists-p custom-path))
;;     (setq user-init-file this-file)
;;     (setq custom-file custom-path)
;;     (load custom-file)))

;; Theme
(setq nano-font-family-monospaced "Iosevka")
(setq nano-font-family-proportional "Iosevka Etoile")
(setq nano-font-size 19)
(require 'nano-faces)
(require 'nano-theme)
(nano-faces)
(nano-theme)
(require 'nano-theme-light)

;; Nano default settings (optional)
;; (require 'nano-defaults)

;; Nano session saving (optional)
;; (require 'nano-session)

;; Nano header & mode lines (optional)
(require 'nano-modeline)

;; Nano key bindings modification (optional)
;; (require 'nano-bindings)

;; Compact layout (need to be loaded after nano-modeline)
(when (member "-compact" command-line-args)
  (require 'nano-compact))

(require 'smex) ; Not needed if you use package.el
(smex-initialize) ; Can be omitted. This might cause a (minimal) delay
                    ; when Smex is auto-initialized on its first run.
(global-set-key (kbd "M-x") 'smex)
(global-set-key (kbd "M-X") 'smex-major-mode-commands)
  ;; This is your old M-x.
(global-set-key (kbd "C-c C-c M-x") 'execute-extended-command)

;; Nano counsel configuration (optional)
;; Needs "counsel" package to be installed (M-x: package-install)
;; (require 'nano-counsel)

;; Welcome message (optional)
(let ((inhibit-message t))
  (message "Welcome to GNU Emacs / N Λ N O edition")
  (message (format "Initialization time: %s" (emacs-init-time))))

;; Splash (optional)
;; (unless (member "-no-splash" command-line-args)
  ;; (require 'nano-splash))

;; Help (optional)
;; (unless (member "-no-help" command-line-args)
;;  (require 'nano-help))

(provide 'nano)

(setq display-line-numbers-type 'relative)
```


### Misc {#misc}

<https://www.cheng92.com/emacs/doom-emacs-with-org/>

```emacs-lisp
(setq-default
 fill-column 80
 undo-limit 80000000
 delete-by-moving-to-trash t
 window-combination-resize t
 delete-trailing-lines t
 x-stretch-cursor t
 typescript-indent-level 2
 custom-file (expand-file-name ".custom.el" doom-private-dir))

(when (file-exists-p custom-file)
  (load custom-file))
```


### Distraction free writing {#distraction-free-writing}

-   writeroom mode config <https://github.com/joostkremers/writeroom-mode>
-   the zen module in doom includes both writeroom and mixed-pitch mode)
-   font in mixed-pitch mode is currently set with `nano-font-family-proportional` and not `doom-variable-pitch-font`

<!--listend-->

```emacs-lisp
(setq +zen-text-scale 0.8)
;; (setq writeroom-local-effects #'centered-window-mode)
;; (setq writeroom-full-line-number-width -20)
(setq writeroom-width 0.7)
(setq writeroom-extra-line-spacing 0.1)

```


## Global Editing {#global-editing}


## Graphics {#graphics}

Graphics require GUI Emacs


### PDF functionalities {#pdf-functionalities}


#### [pdf-tools](https://github.com/politza/pdf-tools#compilation) {#pdf-tools}

<https://emacs.stackexchange.com/questions/13314/install-pdf-tools-on-emacs-macosx>

-   first install poppler automake `brew install poppler automake`
-   set pkg\_config path so pkg-config can find certain libraries, such as `export PKG_CONFIG_PATH="/usr/local/opt/qt/lib/pkgconfig"` `export PKG_CONFIG_PATH="/usr/local/opt/icu4c/lib/pkgconfig"`
-   then install epdfinfo `brew tap dunn/emacs`, `brew install pdf-tools --HEAD`
-   update the package in the future: brew upgrade pdf-tools, then delete the old package via list-packages, restart Emacs and then reinstall the package.

<!--listend-->

```emacs-lisp
(use-package pdf-tools
  :config
  (custom-set-variables
   '(pdf-tools-handle-upgrades nil))    ; Use brew upgrade pdf-tools instead.
  (setq pdf-info-epdfinfo-program "/usr/local/bin/epdfinfo")
  (setq pdf-view-continuous 't)         ; continuous scroll
  )
(pdf-tools-install)
```


#### [org-noter](https://github.com/weirdNox/org-noter#customization-) {#org-noter}

```emacs-lisp
(setq org-noter-set-auto-save-last-location t)
```


### <span class="org-todo todo TODO">TODO</span> Image and vector {#image-and-vector}


## Org Mode Backends {#org-mode-backends}


### Org setup {#org-setup}

references: <https://github.com/sk8ingdom/.emacs.d/blob/master/org-mode-config/org-todo-states.el>

```emacs-lisp
;; (add-hook 'org-mode-hook 'turn-on-auto-fill)

  (after! org
    ;; prompt to record time and note when a task is completed
    (setq org-log-done 'note)

    ;; prompt to record time and note when the scheduled date of a task is modified
    ;; (setq org-log-reschedule 'note)

    ;; promopt to record time and note when the deadline of a task is modified
    ;; (setq org-log-redeadline 'note)

    ;; promopt to record time and note when clocking out of a task
    ;; (setq org-log-clock-out 'note)

    ;; set sorting of items to priority first
    ;; (setq org-lowest-priority ?E) ;; the default priority level only consists of A B C, so E will always be the lowest
    ;; (setq org-default-priority ?E) ;; the regular todos will be set to E by default, meaning no priority

    ;; set the scope of line-editing behavior to the visual line (not actual line)
    (setq vim-style-visual-line-move-text t)

    (setq org-src-fontify-natively t
          org-src-tab-acts-natively t
          org-confirm-babel-evaluate nil
          org-edit-src-content-indentation 0)

    (setq org-enforce-todo-dependencies t) ;;  parent TODO task should not be marked as done until all TODO subtasks, or children tasks, are marked as done
    )

(with-eval-after-load 'org
  (add-to-list 'org-modules 'org-habit t))

```


### Org directory {#org-directory}

references: <https://lists.gnu.org/archive/html/emacs-orgmode/2009-10/msg00734.html>

```emacs-lisp

;; set path abbreviation, useful for inserting org links, easier to change parent path later: https://stackoverflow.com/questions/24782184/emacs-org-mode-folder-alias-for-links-in-org-files
(setq org-link-abbrev-alist
      '(("orgdirtxt" . "~/Dropbox/MarkdownNotes/MDNotes/")
      ("orgdirimg" . "~/Dropbox/MarkdownNotes/MDImage/")))
(setq org-directory "~/Dropbox/MarkdownNotes/MDNotes")
(setq org-agenda-directory "~/Dropbox/MarkdownNotes/MDNotes/")

;; include only several files
(setq org-agenda-files (append `(
                                 ,(concat org-agenda-directory "todo.org")
                                 ,(concat org-agenda-directory "orgw.org")
                                 ,(concat org-agenda-directory "pubr.org")
                                 ;; ,(directory-files (expand-file-name org-agenda-directory) t
                                 ;;                   "pub_.org$")
                                 )
                               ))
;; https://www.reddit.com/r/emacs/comments/4366f9/how_do_orgrefiletargets_work/?st=k5lavqh2&sh=571ac002
(setq org-refile-targets
      '((nil :maxlevel . 5)
        (org-agenda-files :maxlevel . 5)))
(setq org-outline-path-complete-in-steps nil)         ; Refile in a single go
(setq org-refile-use-outline-path t)                  ; Show full paths for refiling
;; (setq org-default-notes-file (concat org-directory "/notes.org"))
```


### <span class="org-todo todo TODO">TODO</span> Org files management and interaction {#org-files-management-and-interaction}

full text search, configure relationships (links), visualization of relationships


### Org keywords and tags {#org-keywords-and-tags}

Previously I cannot override these settings because I changed setting in M-x
customization, now I deleted the content in ~/.emacs.d/.local/custom.el and add
after! org, then things are fine: <https://github.com/hlissner/doom-emacs/issues/546>

```emacs-lisp
  (after! org
    (setq org-todo-keywords
          '(;; Sequence for TASKS
            ;; TODO means it's an item that needs addressing
            ;; PEND means it's dependent on something else happening
            ;; CANC means it's no longer necessary to finish
            ;; DONE means it's complete
            (sequence "TODO(t@/!)" "PEND(p@/!)" "|" "DONE(d@/!)" "CANC(c@/!)")

            ;; Sequence for MULTIMEDIA
            ;; MARK mark some media for future consuming
            ;; ING means currently consuming
            ;; REPO means the media has been consumed, and waiting to be shared in short or long form
            ;; ARCV media alrady repoed, now archive for future reference
            ;; (sequence "MARK(m@/!)" "ING(i@/!)" "REPO(r@/!)" "|" "ARCV(a@/!)")

            ;; Sequence for PUBLICATION
            (sequence "WRIT(w@/!)" "PIPE(e@/!)" "|" "IDEA(i@/!)" "ARCV(a@/!)")
            ))
    (setq org-todo-keyword-faces
          '(
            ("TODO" :foreground "Orange")
            ("WRIT" :foreground "Orange")
            ("PIPE" :foreground "LightSlateBlue")
            ("IDEA" :foreground "LightCoral")
            ("PEND" :foreground "dark grey")
            ("DONE" :foreground "dark grey")
            ("CANC" :foreground "dark grey")
            ("ARCV" :foreground "dark grey")
             )
            )
    )
```


### Org calendar {#org-calendar}

reference: <https://github.com/kiwanami/emacs-calfw>

```emacs-lisp

(require 'calfw)
(require 'calfw-ical)
(require 'calfw-org)
(setq cfw:org-overwrite-default-keybinding t)
(setq calendar-week-start-day 1) ; 0:Sunday, 1:Monday
;; (define-key cfw:calendar-mode-map (kbd "<SPC-o>") 'cfw:show-details-command)
;; (define-key cfw:org-custom-map (kbd "SPC") nil)

(setq cfw:fchar-junction ?+
      cfw:fchar-vertical-line ?|
      cfw:fchar-horizontal-line ?-
      cfw:fchar-left-junction ?+
      cfw:fchar-right-junction ?+
      cfw:fchar-top-junction ?+
      cfw:fchar-top-left-corner ?+
      cfw:fchar-top-right-corner ?+)

;; (setq cfw:org-agenda-schedule-args '(:tags-todo))

(defun mycal ()
  (interactive)
  (cfw:open-calendar-buffer
   :view 'week
  :contents-sources
   (list
    (cfw:org-create-source "Orange")
    (cfw:ical-create-source "gcal" "https://calendar.google.com/calendar/ical/huangyizhen2002%40gmail.com/private-78a173f476129e08782df341dc7ab562/basic.ics" "IndianRed")
    )))
```


### Org agenda {#org-agenda}

unicode for symbol: <https://www.ssec.wisc.edu/~tomw/java/unicode.html>


#### global setting {#global-setting}

```emacs-lisp

;; generic setting

;; (setq org-agenda-block-separator 9596)
;; (setq org-agenda-skip-scheduled-if-done t)
(setq org-agenda-start-with-follow-mode t)

;; set week view to center around today, 7 day in furture, 0 day in past https://stackoverflow.com/questions/32423127/how-to-view-the-next-days-in-org-modes-agenda
;; (setq org-agenda-span 1
;;       org-agenda-start-on-weekday nil
;;       org-agenda-start-day "-0d")
```


#### custom agenda {#custom-agenda}

-   related task and inspiration [customize agenda view further]({{< relref "orgw" >}})
-   ref: set custom agenda view
    -   <https://stackoverflow.com/questions/17003338/emacs-org-mode-how-to-find-all-todos-that-dont-have-a-deadline-specified>
    -   <https://orgmode.org/manual/Custom-Agenda-Views.html>
-   ref: custom agenda view and super-agenda group items by groups
    -   <https://github.com/alphapapa/org-super-agenda/blob/master/examples.org>
    -   <https://github.com/novoid/dot-emacs/blob/master/config.org>
    -   <https://emacs.christianbaeuerlein.com/my-org-config.html>
    -   <https://emacs.stackexchange.com/questions/52994/org-mode-agenda-show-list-of-tasks-done-in-the-past-and-not-those-clocked>
-   currently my custom agenda views:
    -   weekly planning view: a simple week view of all scheduled todo without super agenda categories
    -   weekly review view: a week view of only done headings (not clocked)
    -   daily agenda: super-agenda category, follow mode,

<!--listend-->

```emacs-lisp

;; enable super agenda, but disable its key map
(org-super-agenda-mode)
(setq org-super-agenda-header-map nil)

(setq org-agenda-custom-commands
      '(
        ("g" . "Global Agendas")
        ("gi" "[i]nbox but unscheduled" tags-todo "+inb-SCHEDULED={.+}"
         ((org-agenda-sorting-strategy '(tag-up))))
        ("w" . "Weekview Agendas")
        ("wa" "[a]genda weekly"
         agenda "" (
                   (org-agenda-start-day "-0d")
                   (org-agenda-span 7)
                   (org-agenda-start-on-weekday nil)
                   (org-agenda-prefix-format '((agenda . "  %?-12t")))
                   (org-habit-show-habits nil)
                   (org-agenda-use-time-grid nil)
                   (org-agenda-time-grid '((daily)
                                           (800 1000 1200 1400 1600 1800 2000)
                                           "......" "----------------"))
                   (org-agenda-skip-function '(org-agenda-skip-entry-if 'todo '("CANC" "DONE")))
                   ))
        ("wr" "[r]eview weekly"
         agenda "" (
                   (org-agenda-start-day "-6d")
                   (org-agenda-span 7)
                   (org-agenda-compact-blocks t)
                   (org-agenda-prefix-format '((agenda . "  %?-12t")))
                   (org-agenda-start-with-log-mode "clockcheck")
                   (org-agenda-log-mode-items '(closed clock))
                   (org-agenda-start-with-clockreport-mode t)
                   (org-agenda-skip-function '(org-agenda-skip-entry-if 'todo '("TODO" "PEND")))
                   ))
      ("d" . "Dayview Agendas")
        ("da" "[a]genda day"
         (
          (agenda "" (
                      (org-agenda-start-day "-0d")
                      (org-agenda-span 1)
                      (org-agenda-start-on-weekday nil)
                      (org-agenda-overriding-header "")
                      (org-agenda-prefix-format '((agenda . "  %?-12t")))
                      (org-agenda-start-with-log-mode nil)
                      (org-agenda-skip-deadline-prewarning-if-scheduled t)
                      (setq org-agenda-block-separator 9596)
                      (org-super-agenda-groups
                       '(
                         (:name "Today"
                          :time-grid t
                          :habit t)
                         ;; (:name "H" :tag "H")
                         (:name "P"
                          :face (:box (:line-width 1 :color "chocolate"))
                          :and (:todo "TODO" :category ("pub1")))
                         (:name "Z"
                          :and (:todo "TODO"  :tag "Z"))
                         (:name "S"
                          :and (:todo "TODO"  :tag "S"))
                         (:name "X"
                          :and (:todo "TODO"  :tag "X"))
                         (:name "Done today"
                          :and (:regexp "State \"DONE\""
                                :log t))
                         (:discard (:anything t))
                       ))))
          (alltodo "" (
                      (org-agenda-overriding-header "")
                      (org-agenda-prefix-format '((agenda . "  %?-12t")))
                      (org-super-agenda-groups
                       '(
                         (:name "Waiting..."
                          :face (:foreground "dark gray")
                          :and (:todo "PEND" :not(:tag "ant")))
                         (:name "Due soon"
                         :face (:foreground "dark gray")
                         :deadline future)
                         ;; (:discard (:not (:todo "TODO")))
                         (:discard (:anything t))
                         ))))
                  )))
)

```


#### refresh agenda {#refresh-agenda}

<https://www.cheng92.com/emacs/doom-emacs-with-org/>

```emacs-lisp

;;;###autoload
(defun aj-org-agenda-save-and-refresh-a (&rest _)
  "Save org files and refresh.
Only org files contributing to `org-agenda' are saved.
Refreshed are `org-agenda' org `org-ql-view', depending on
which one is currently active."
  (org-save-all-org-buffers)
  (if (string-match "Org QL" (buffer-name))
      (org-ql-view-refresh)
    (org-agenda-redo)))
```


### Org roam {#org-roam}

<https://www.cheng92.com/emacs/doom-emacs-with-org/>

```emacs-lisp
(setq org-roam-v2-ack t
      org-roam-directory "~/Dropbox/MarkdownNotes/Zettel")

(use-package org-roam
  :config
  (setq
   org-roam-file-extensions '("org")
   org-roam-capture-templates
   (quote
    (("d" "default" plain
      (function org-roam-capture--get-point)
      "%?" :file-name "%<%Y_%m%d>_${slug}"
      :head "#+TITLE: ${title}\n\n" :unnarrowed t)))
   )
  ; (org-roam-setup)
  ; :bind (("C-c r l" . org-roam-buffer-toggle)
  ;        ("C-c r f" . org-roam-node-find)
  ;        ("C-c r g" . org-roam-graph)
  ;        ("C-c r i" . org-roam-node-insert)
  ;        ("C-c r c" . org-roam-capture)
  ;        ("C-c r j" . org-roam-dailies-capture-today)
  ;        )
         )
```


### Org timer/clock {#org-timer-clock}

ref: <https://emacs.stackexchange.com/questions/34746/how-to-get-an-audible-feedback-when-the-current-task-is-overrun>
<https://github.com/leoliu/play-sound-osx>
<https://systemcrafters.cc/emacs-shorts/pomodoro-timer/>

```emacs-lisp
;; (unless (and (fboundp 'play-sound-internal)
;;              (subrp (symbol-function 'play-sound-internal)))
;;   (require 'play-sound))
;; (setq ring-bell-function (lambda ()
;;                           (play-sound-file "~/Dropbox/windchime.wav")))
;; (setq org-clock-sound "~/Dropbox/windchime.wav")

;; Format org-mode clocktables the way we want to include Effort
;; In the clocktable header:
;; :formatter my/clocktable-write

(defun my/clocktable-write (&rest args)
  "Custom clocktable writer.
Uses the default writer but shifts the first column right 3 columns,
and names the estimation error column."
  (apply #'org-clocktable-write-default args)
  (save-excursion
    (forward-char) ;; move into the first table field
    (org-table-move-column-right)
    (org-table-move-column-right)
    (org-table-move-column-right)
    (org-table-next-field)
    (insert "Est. error")
    (org-table-previous-field)))

```


### Org capture {#org-capture}

references:
<https://emacs.stackexchange.com/questions/19391/can-t-set-directory-for-org-mode-capture>
<https://orgmode.org/manual/Template-expansion.html#Template-expansion>
<https://github.com/sk8ingdom/.emacs.d/blob/master/org-mode-config/org-capture-templates.el>


#### capture template {#capture-template}

```emacs-lisp
  (setq org-capture-templates
        '(
          ("t" "Todo" entry (file+headline "~/Dropbox/MarkdownNotes/MDNotes/todo.org" "Inbox") "* TODO %?\n  %U\n")
          ("o" "Org" entry (file+headline "~/Dropbox/MarkdownNotes/MDNotes/orgw.org" "Feature wish list") "* TODO %?\n  %U\n")
          ("p" "tjp-Todo" entry (file+headline "~/Dropbox/MarkdownNotes/MDNotes/pub_vr_stress.org" "TODO manuscript v1 :Z:vr:") "* TODO %?\n  %U\n")
          ("d" "Drafts" entry (file+headline "~/Dropbox/MarkdownNotes/MDNotes/todo.org" "Drafts") "* ARCV %?\n  %U\n")
          ("j" "Journal" entry (function org-journal-find-location)
           "* %(format-time-string org-journal-time-format)%^{Title}\n%i%?")
          )
        )
```


#### org capture in Alfred;; Alfred script {#org-capture-in-alfred-alfred-script}

adapted from <https://github.com/jjasghar/alfred-org-capture/pull/1>

```emacs-lisp

;; make new capture frame from alfred
(defun make-orgcapture-frame ()
  "Create a new frame and run org-capture."
  (interactive)
  (make-frame '((name . "temp") (width . 80) (height . 16)
                (top . 400) (left . 300)
                (font . "-apple-Monaco-medium-normal-normal-*-13-*-*-*-m-0-iso10646-1")
                ))
  (select-frame-by-name "temp")
  (org-capture)
  )

;; Use org-mac to grab various application link and insert it to the captured item
(add-hook 'org-capture-prepare-finalize-hook
          (lambda ()
            (when (equal
                   (cdr (assoc 'name (frame-parameters (selected-frame))))
                   "temp")
              (progn
                (goto-char (point-max))
                (call-interactively 'org-mac-grab-link)))))

;; Delete frame when capture is done
(add-hook 'org-capture-after-finalize-hook
          (lambda ()
            (when (equal
                   (cdr (assoc 'name (frame-parameters (selected-frame))))
                   "temp")
              (delete-frame))))


```


### org-journal {#org-journal}

references: <https://github.com/bastibe/org-journal>

```emacs-lisp
;; (customize-set-variable 'org-journal-dir "~/Dropbox/Markdownnotes/MDNotes/")
;; (customize-set-variable 'org-journal-date-format "%A, %d %B %Y")
;; (customize-set-variable 'org-journal-file-format "%Y%m%d.org")
(require 'org-journal)

;; check custom.el when changing these vairables
(use-package org-journal
  ;; :ensure t
  :defer t
  :custom
  (org-journal-dir "~/Dropbox/MarkdownNotes/MDNotes/")
  (org-journal-file-format "%Y%m%d.org")
  (org-journal-date-format "%A, %B %d %Y"))

(defun org-journal-find-location ()
  ;; Open today's journal, but specify a non-nil prefix argument in order to
  ;; inhibit inserting the heading; org-capture will insert the heading.
  (org-journal-new-entry t)
  ;; Position point on the journal's top-level heading so that org-capture
  ;; will add the new entry as a child entry.
  (goto-char (point-min)))

```


### org-babel {#org-babel}

Write plain text in org mode with embedded source block supported by [Babel: active code in Org-mode](https://orgmode.org/worg/org-contrib/babel/index.html)
also see <https://orgmode.org/manual/Working-with-Source-Code.html>

```emacs-lisp
  (setq org-babel-load-languages
        '(
          (emacs-lisp. t)
          (lisp. t)
          (sh. t)
          (org. t)
          (python. t)
          (latex. t)
          (R. t)))

;; activate Babel languages
(org-babel-do-load-languages
 'org-babel-load-languages
 org-babel-load-languages)

  ;; (add-hook 'org-babel-after-execute-hook 'org-display-inline-images)
  ;; (add-hook 'org-mode-hook 'org-display-inline-images)
```


### taskjuggler {#taskjuggler}

<https://orgmode.org/worg/exporters/taskjuggler/ox-taskjuggler.html>

```emacs-lisp
(require 'ox-taskjuggler)
(setq org-taskjuggler-valid-resource-attributes '(limits leaves vacation shift booking efficiency journalentry rate workinghours flags))
(setq org-taskjuggler-keep-project-as-task nil)
(setq org-taskjuggler-project-tag "tjp")
(setq org-taskjuggler-resource-tag "tjr")
(setq org-taskjuggler-default-project-duration 720)
(setq org-taskjuggler-default-reports
      '(
        "textreport report \"Plan\" {
  formats html
  header '== %title =='
  center -8<-
    [#Plan Plan] |  [#Ongoing Ongoing] | [#Resource_Allocation Resource Allocation]
    ----
    === Plan ===
    <[report id=\"plan\"]>
    ----
    === Ongoing ===
    <[report id=\"ongoing\"]>
    ----
    === Resource Allocation ===
    <[report id=\"resourceGraph\"]>
  ->8-
}

# A traditional Gantt chart with a project overview.
taskreport plan \"\" {
  scenarios plan
  headline \"Project Plan\"
  columns bsi, name, start, end, effort, complete, chart
  loadunit shortauto
  hideresource 1
}

# Added tasks that are not complete
taskreport ongoing \"\" {
    headline \"Ongoing Tasks\"
    columns bsi, name, start, end, effort, complete, chart
    hidetask ~(plan.complete != 100)
  }

# A graph showing resource allocationd
resourcereport resourceGraph \"\" {
  headline \"Resource Allocation Graph\"
  columns no, name, effort, weekly
  loadunit shortauto
  hidetask ~(isleaf() & isleaf_())
  sorttasks plan.start.up
}"))

```


### exporting {#exporting}


#### generic {#generic}

```emacs-lisp
(setq org-export-with-broken-links t)
```


#### pandoc {#pandoc}

-   pandoc-import: convert non-org files to org <https://github.com/tecosaur/org-pandoc-import>
-   pandoc-export: <https://github.com/kawabata/ox-pandoc>

<!--listend-->

```emacs-lisp

(use-package org-pandoc-import :after org)

```


### dblock {#dblock}

-   <https://orgmode.org/manual/Dynamic-Blocks.html#Dynamic-Blocks>

<!--listend-->

```emacs-lisp
;; update all dynamic blocks upon saving
(add-hook 'before-save-hook 'org-update-all-dblocks)
```


### create unique custom id {#create-unique-custom-id}

<span class="timestamp-wrapper"><span class="timestamp">[2021-10-06 Wed 14:41] </span></span> <- [manage and visualize linked notes]({{<relref "../../Dropbox/MarkdownNotes/MDNotes/orgw.md#" >}})

-   source:
    -   generate readable custom id from heading (my-id-get-or-generate) <https://github.com/novoid/dot-emacs/blob/master/config.org>
    -   generate readable custom id from invoking counsel (zz/) <https://zzamboni.org/post/how-to-easily-create-and-use-human-readable-ids-in-org-mode-and-doom-emacs/>
-   instructions <https://karl-voit.at/2019/11/16/UOMF-Linking-Headings/>
    -   Go to two target headings. It doesn't have to be the heading line.
    -   Get the ID by invoking my-id-get-or-generate. If the heading did not have an :ID: property, it gets generated.
    -   Use super-links-store/insert link
    -   Follow link by `<C-c C-o>` or `RET`

<!--listend-->

```emacs-lisp

(defun my-generate-sanitized-alnum-dash-string(str)
"Returns a string which contains only a-zA-Z0-9 with single dashes
 replacing all other characters in-between them.

 Some parts were copied and adapted from org-hugo-slug
 from https://github.com/kaushalmodi/ox-hugo (GPLv3)."
(let* (;; Remove "<FOO>..</FOO>" HTML tags if present.
       (str (replace-regexp-in-string "<\\(?1:[a-z]+\\)[^>]*>.*</\\1>" "" str))
       ;; Remove URLs if present in the string.  The ")" in the
       ;; below regexp is the closing parenthesis of a Markdown
       ;; link: [Desc](Link).
       (str (replace-regexp-in-string (concat "\\](" ffap-url-regexp "[^)]+)") "]" str))
       ;; Replace "&" with " and ", "." with " dot ", "+" with
       ;; " plus ".
       (str (replace-regexp-in-string
             "&" " and "
             (replace-regexp-in-string
              "\\." " dot "
              (replace-regexp-in-string
               "\\+" " plus " str))))
       ;; Replace German Umlauts with 7-bit ASCII.
       (str (replace-regexp-in-string "[Ä]" "Ae" str t))
       (str (replace-regexp-in-string "[Ü]" "Ue" str t))
       (str (replace-regexp-in-string "[Ö]" "Oe" str t))
       (str (replace-regexp-in-string "[ä]" "ae" str t))
       (str (replace-regexp-in-string "[ü]" "ue" str t))
       (str (replace-regexp-in-string "[ö]" "oe" str t))
       (str (replace-regexp-in-string "[ß]" "ss" str t))
       ;; Replace all characters except alphabets, numbers and
       ;; parentheses with spaces.
       (str (replace-regexp-in-string "[^[:alnum:]()]" " " str))
       ;; On emacs 24.5, multibyte punctuation characters like "："
       ;; are considered as alphanumeric characters! Below evals to
       ;; non-nil on emacs 24.5:
       ;;   (string-match-p "[[:alnum:]]+" "：")
       ;; So replace them with space manually..
       (str (if (version< emacs-version "25.0")
                (let ((multibyte-punctuations-str "：")) ;String of multibyte punctuation chars
                  (replace-regexp-in-string (format "[%s]" multibyte-punctuations-str) " " str))
              str))
       ;; Remove leading and trailing whitespace.
       (str (replace-regexp-in-string "\\(^[[:space:]]*\\|[[:space:]]*$\\)" "" str))
       ;; Replace 2 or more spaces with a single space.
       (str (replace-regexp-in-string "[[:space:]]\\{2,\\}" " " str))
       ;; Replace parentheses with double-hyphens.
       (str (replace-regexp-in-string "\\s-*([[:space:]]*\\([^)]+?\\)[[:space:]]*)\\s-*" " -\\1- " str))
       ;; Remove any remaining parentheses character.
       (str (replace-regexp-in-string "[()]" "" str))
       ;; Replace spaces with hyphens.
       (str (replace-regexp-in-string " " "-" str))
       ;; Remove leading and trailing hyphens.
       (str (replace-regexp-in-string "\\(^[-]*\\|[-]*$\\)" "" str)))
  str)
)

(map!
   (:after evil
     :m  "C-c i d" #'my-id-get-or-generate))


(defun my-id-get-or-generate()
"Returns the ID property if set, or generates and returns a new one if not set.
 The generated ID is stripped off potential progress indicator cookies and
 sanitized to get a slug. Furthermore, it is prepended with an ISO date-stamp
 if none was found before."
    (interactive)
        (when (not (org-id-get))
            (progn
               (let* (
                      (my-heading-text (nth 4 (org-heading-components)))
                      (my-heading-text (replace-regexp-in-string "[[][0-9%/]+[]] " "" my-heading-text))
                      (new-id (my-generate-sanitized-alnum-dash-string my-heading-text))
                     )
                   (when (not (string-match "[12][0-9][0-9][0-9]-[01][0-9]-[0123][0-9]-.+" new-id))
                           (setq new-id (concat (format-time-string "%Y-%m-%d-") new-id)))
                   (org-set-property "ID" new-id)
                   )
                 )
        )
        (kill-new (concat "id:" (org-id-get)))
        (org-id-get)
        )

(defun zz/make-id-for-title (title)
  "Return an ID based on TITLE."
  (let* ((new-id (replace-regexp-in-string "[^[:alnum:]]" "-" (downcase title))))
    new-id))

(defun zz/org-custom-id-create ()
  "Create and store CUSTOM_ID for current heading."
  (let* ((title (or (nth 4 (org-heading-components)) ""))
         (new-id (zz/make-id-for-title title)))
    (org-entry-put nil "CUSTOM_ID" new-id)
    (org-id-add-location new-id (buffer-file-name (buffer-base-buffer)))
    new-id))

(defun zz/org-custom-id-get-create (&optional where force)
  "Get or create CUSTOM_ID for heading at WHERE.

If FORCE is t, always recreate the property."
  (org-with-point-at where
    (let ((old-id (org-entry-get nil "CUSTOM_ID")))
      ;; If CUSTOM_ID exists and FORCE is false, return it
      (if (and (not force) old-id (stringp old-id))
          old-id
        ;; otherwise, create it
        (zz/org-custom-id-create)))))

;; Now override counsel-org-link-action
(after! counsel
  (defun counsel-org-link-action (x)
    "Insert a link to X.

X is expected to be a cons of the form (title . point), as passed
by `counsel-org-link'.

If X does not have a CUSTOM_ID, create it based on the headline
title."
    (let* ((id (zz/org-custom-id-get-create (cdr x))))
      (org-insert-link nil (concat "#" id) (car x)))))

```


### auto backlinks with org-super-links {#auto-backlinks-with-org-super-links}

<span class="timestamp-wrapper"><span class="timestamp">[2021-10-06 Wed 14:41] </span></span> <- [manage and visualize linked notes]({{<relref "../../Dropbox/MarkdownNotes/MDNotes/orgw.md#" >}})

-   generate id with my-id-get-or-generate, then use super-links
-   <https://github.com/toshism/org-super-links>

<!--listend-->

```emacs-lisp
(use-package org-super-links
  :bind (
         ("C-c s g" . org-super-links-link)
         ("C-c s s" . org-super-links-store-link)
         ("C-c s l" . org-super-links-insert-link)
         ("C-c s d" . org-super-links-delete-link)
         )
  :config
  (require 'org-id)
  (setq org-id-link-to-org-use-id 'create-if-interactive-and-no-custom-id)
  ;; create unique id if no custom id is found (custom id created by the my-id-get function)
  (setq org-super-links-related-into-drawer t))
```


### org outline {#org-outline}

<https://www.cheng92.com/emacs/doom-emacs-with-org/>

```emacs-lisp
(use-package org-ol-tree
  :commands org-ol-tree)

(map! :map org-mode-map
    :after org
    :localleader
    :desc "Outline" "O" #'org-ol-tree)
```


## Org Mode Looks {#org-mode-looks}


### Change the character that displays on collapsed headings {#change-the-character-that-displays-on-collapsed-headings}

```emacs-lisp
  (setq org-ellipsis " ▼ ")
```


### Line wrapping and centering {#line-wrapping-and-centering}

```emacs-lisp
;; visual line mode will wrap lines at the window border without actually insert line breaks
(add-hook 'org-mode-hook #'visual-line-mode)

;; turn off auto fill mode so there won't be any hard line breaks after the wrap column
(add-hook 'org-mode-hook #'turn-off-auto-fill)

;; also add (setq evil-respect-visual-line-mode t) to init.el, enable navigation in visual line mode (evel-next-visual-line)
(add-hook 'org-mode-hook #'adaptive-wrap-prefix-mode)

;; center the text in org mode
(add-hook 'org-mode-hook #'centered-window-mode)

```


## Org Mode Editing {#org-mode-editing}


### Key binding for navigation {#key-binding-for-navigation}


#### jumping to the end of the line {#jumping-to-the-end-of-the-line}

currently C-e binds to evil-scroll-lines-down, it's not consistent with org mode

```emacs-lisp
(map!
   (:after evil
     :m  "C-e" #'doom/forward-to-last-non-comment-or-eol))
```


#### section navigation {#section-navigation}

-   use [ for navigating all headings, to be consistent with the existing key binding:
    -   `org-forward-heading-same-level` <] h>
    -   `org-forward-paragraph` <}>
    -   `evil-org-forward-sentence` <)>

<!--listend-->

```emacs-lisp

(map!
   (:after org
    :m  "[ [" #'org-previous-visible-heading
    :m  "] ]" #'org-next-visible-heading
    :m "] -" #'org-next-item
    :m "[ -" #'org-previous-item
    :m "] ~" #'org-next-block
    :m "[ ~" #'org-previous-block
    ))

```


### Key binding for showing all TODO headings {#key-binding-for-showing-all-todo-headings}

```emacs-lisp
  (global-set-key (kbd "C-c t") 'org-show-todo-tree)
```


### Key binding for outline quicklook/goto {#key-binding-for-outline-quicklook-goto}

```emacs-lisp
  (global-set-key (kbd "C-c r") 'counsel-outline)
```


### Key binding to insert newlines above and below {#key-binding-to-insert-newlines-above-and-below}

Similar to [vim-unimpaired](https://github.com/tpope/vim-unimpaired)

```emacs-lisp

  (map!
   (:after evil
     :m  "] SPC" #'evil-motion-insert-newline-below
     :m  "[ SPC" #'evil-motion-insert-newline-above))

```


### Key binding for save all Org files {#key-binding-for-save-all-org-files}

```emacs-lisp

(map!
   :after evil
   :m  "C-x s" #'org-save-all-org-buffers)

```


### Function to duplicate certain line {#function-to-duplicate-certain-line}

inspired by Sublime Text cmd + shift + d (ref: <https://stackoverflow.com/a/88828>)

```emacs-lisp
(defun duplicate-line()
    (interactive)
    (move-beginning-of-line 1)
    (kill-line)
    (yank)
    (open-line 1)
    (forward-line 1)
    (yank)
    )
(global-set-key (kbd "C-c D") 'duplicate-line)
```


### Function to create new scratch buffer in Org {#function-to-create-new-scratch-buffer-in-org}

ref: <https://emacs.stackexchange.com/questions/16492/is-it-possible-to-create-an-org-mode-scratch-buffer>

```emacs-lisp
(defun org-buffer-new ()
"Create a new scratch buffer -- \*hello-world\*"
(interactive)
  (let ((n 0)
        bufname buffer)
    (catch 'done
      (while t
        (setq bufname (concat "*org-scratch"
          (if (= n 0) "" (int-to-string n))
            "*"))
        (setq n (1+ n))
        (when (not (get-buffer bufname))
          (setq buffer (get-buffer-create bufname))
          (with-current-buffer buffer
            (org-mode))
          ;; When called non-interactively, the `t` targets the other window (if it exists).
          (throw 'done (display-buffer buffer t))) ))))

  ;; SPC x is the default doom new scratch buffer key binding
  (global-set-key (kbd "C-c x") 'org-buffer-new)
```


### Auto update Org toc {#auto-update-org-toc}

```emacs-lisp
(if (require 'toc-org nil t)
    (add-hook 'org-mode-hook 'toc-org-mode)

    ;; enable in markdown, too
    (add-hook 'markdown-mode-hook 'toc-org-mode))
```


### Ignore headings but not contents when exporting {#ignore-headings-but-not-contents-when-exporting}

ignore any headings with tag :ignore:, but not its section content
ref: <https://emacs.stackexchange.com/questions/9492/is-it-possible-to-export-content-of-subtrees-without-their-headings>

```emacs-lisp
 (require 'ox-extra)
 (ox-extras-activate '(ignore-headlines))
```


### Function to copy subtree contents but not headings {#function-to-copy-subtree-contents-but-not-headings}

the default org-copy-subtree will yank all the subtree contents including heading and section text, this function only yank the section text of this heading but not the heading or any subheadings and their contents
ref: <https://hungyi.net/posts/org-mode-subtree-contents/>

```emacs-lisp
(defun org-copy-subtree-contents ()
  "Get the content text of the subtree at point and add it to the `kill-ring'.
Excludes the heading and any child subtrees."
  (interactive)
  (if (org-before-first-heading-p)
      (message "Not in or on an org heading")
    (save-excursion
      ;; If inside heading contents, move the point back to the heading
      ;; otherwise `org-agenda-get-some-entry-text' won't work.
      (unless (org-on-heading-p) (org-previous-visible-heading 1))
      (let ((contents (substring-no-properties
                       (org-agenda-get-some-entry-text
                        (point-marker)
                        most-positive-fixnum))))
        (message "Copied: %s" contents)
        (kill-new contents)))))
```


## Markdown Mode Looks {#markdown-mode-looks}

```emacs-lisp
;; similar with org mode, use visual line mode without auto fill
(add-hook 'markdown-mode-hook #'visual-line-mode)
(add-hook 'markdown-mode-hook #'turn-off-auto-fill)

;; disable syntax checker (markdownlint-cli) for markdown mode
(setq-default flycheck-disabled-checkers '(markdown-markdownlint-cli))
```


## Hugo Related Config {#hugo-related-config}

references:
<https://ox-hugo.scripter.co/>
<https://mstempl.netlify.com/post/static-website-with-emacs-and-hugo/>

```emacs-lisp
(with-eval-after-load 'ox
  (require 'ox-hugo))
```


## R Related Config {#r-related-config}


### ESS R config {#ess-r-config}

references: <https://iqss.github.io/IQSS.emacs/init.html#run%5Fr%5Fin%5Femacs%5F(ess)>

```emacs-lisp
(with-eval-after-load "ess"
    (require 'ess-site)
    (require 'ess-mode)
;; standard control-enter evaluation
  (define-key ess-mode-map (kbd "<C-S-return>") 'ess-eval-region-or-function-or-paragraph-and-step)
  (define-key ess-mode-map (kbd "<C-return>") 'ess-eval-line-and-step)
  ;; (define-key ess-mode-map [remap ess-indent-or-complete] #'company-indent-or-complete-common)
    ;; Set ESS options
    (setq
     ess-auto-width 'window
     ess-use-auto-complete nil
     ess-use-company 't
     ;; ess-r-package-auto-set-evaluation-env nil
     inferior-ess-same-window 't
   ess-indent-with-fancy-comments nil ; don't indent comments
     ess-eval-visibly t                 ; enable echoing input
     ess-eval-empty t                   ; don't skip non-code lines.
     ess-ask-for-ess-directory nil ; start R in the working directory by default
     ess-R-font-lock-keywords      ; font-lock, but not too much
     (quote
      ((ess-R-fl-keyword:modifiers)
       (ess-R-fl-keyword:fun-defs . t)
       (ess-R-fl-keyword:keywords . t)
       (ess-R-fl-keyword:assign-ops  . t)
       (ess-R-fl-keyword:constants . 1)
       (ess-fl-keyword:fun-calls . t)
       (ess-fl-keyword:numbers)
       (ess-fl-keyword:operators . t)
       (ess-fl-keyword:delimiters)
       (ess-fl-keyword:=)
       (ess-R-fl-keyword:F&T))))
      ;; (define-key evil-normal-state-map (kbd "<SPC-e>") 'ess-execute))
      )
```


### Combine R and markdown mode in one buffer with polymode {#combine-r-and-markdown-mode-in-one-buffer-with-polymode}

```emacs-lisp
  (use-package polymode)
  (use-package poly-R)
  (use-package poly-markdown
    :config
    (add-to-list 'auto-mode-alist '("\\.rmd" . poly-markdown+R-mode))
    )
(with-eval-after-load "markdown"
  (use-package poly-markdown))
(with-eval-after-load "org"
  (use-package poly-org))
```


## Feeds Related Config {#feeds-related-config}


### elfeed {#elfeed}

```emacs-lisp

(setq elfeed-feeds
      '("https://rsshub.app/hackernews/best/comments"
"https://rsshub.app/douban/book/rank/fiction"
        ))
```


### email {#email}
