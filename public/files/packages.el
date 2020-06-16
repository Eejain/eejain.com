;; -*- no-byte-compile: t; -*-
;;; $DOOMDIR/packages.el

;; To install a package with Doom you must declare them here, run 'doom sync' on
;; the command line, then restart Emacs for the changes to take effect.
;; Alternatively, use M-x doom/reload.
;;
;; WARNING: Disabling core packages listed in ~/.emacs.d/core/packages.el may
;; have nasty side-effects and is not recommended.


;; All of Doom's packages are pinned to a specific commit, and updated from
;; release to release. To un-pin all packages and live on the edge, do:
;(unpin! t)

;; ...but to unpin a single package:
;(unpin! pinned-package)
;; Use it to unpin multiple packages
;(unpin! pinned-package another-pinned-package)


;; To install SOME-PACKAGE from MELPA, ELPA or emacsmirror:
;(package! some-package)

;; To install a package directly from a particular repo, you'll need to specify
;; a `:recipe'. You'll find documentation on what `:recipe' accepts here:
;; https://github.com/raxod502/straight.el#the-recipe-format
;(package! another-package
;  :recipe (:host github :repo "username/repo"))

;; If the package you are trying to install does not contain a PACKAGENAME.el
;; file, or is located in a subdirectory of the repo, you'll need to specify
;; `:files' in the `:recipe':
;(package! this-package
;  :recipe (:host github :repo "username/repo"
;           :files ("some-file.el" "src/lisp/*.el")))

;; If you'd like to disable a package included with Doom, for whatever reason,
;; you can do so here with the `:disable' property:
;(package! builtin-package :disable t)

;; You can override the recipe of a built in package without having to specify
;; all the properties for `:recipe'. These will inherit the rest of its recipe
;; from Doom or MELPA/ELPA/Emacsmirror:
;(package! builtin-package :recipe (:nonrecursive t))
;(package! builtin-package-2 :recipe (:repo "myfork/package"))

;; Specify a `:branch' to install a package from a particular branch or tag.
;; This is required for some packages whose default branch isn't 'master' (which
;; our package manager can't deal with; see raxod502/straight.el#279)
;(package! builtin-package :recipe (:branch "develop"))

(package! term-cursor
  :recipe (:host github :repo "h0d/term-cursor.el"))

(package! osx-clipboard
  :recipe (:host github :repo "joddie/osx-clipboard-mode"))

;; (package! centered-window
;;   :recipe (:host github :repo "anler/centered-window-mode"))

(package! org-journal)
(package! adaptive-wrap)
(package! ox-hugo)
;; (package! org-agenda-property)
(package! polymode)
(package! poly-markdown)
(package! poly-R)
(package! poly-org)
;; (package! simpleclip)
;; (package! cliphist)
;; (package! sublimity)
(package! toc-org)
(package! psession)


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
(package! doom-modeline)
(package! anzu)
(package! evil-anzu)

;; ui/indent-guides
; (package! highlight-indent-guides)
