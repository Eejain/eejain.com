+++
title = "My Doom Emacs Configuration"
author = ["Eejain Huang"]
date = 2020-06-16
tags = ["technology"]
draft = false
bookHidden = true
+++

## Intro {#intro}


### Forewords {#forewords}

-   A few words about my background. I'm a researcher in the field of social science. I don't program much, but Emacs is essential for both my work and leisure projects. I'm using Emacs for interacting with R environment, composing long format texts, and managing my schedule as well as logging everyday life.
-   I'm using [GNU Emacs](https://formulae.brew.sh/formula/emacs) under the [Doom Emacs](https://github.com/hlissner/doom-emacs) configuration framework. I'm running my Emacs in[ iTerm2](https://www.iterm2.com/)--a terminal emulator for macOS.


### Instructions {#instructions}

-   Run the following in the terminal. References: <https://github.com/hlissner/doom-emacs/blob/develop/docs/getting%5Fstarted.org>

<!--listend-->

```sh
brew install git ripgrep
brew install coreutils fd
xcode-select --install
brew install emacs
git clone https://github.com/hlissner/doom-emacs ~/.emacs.d
~/.emacs.d/bin/doom install
```

-   Add `export PATH=~/.emacs.d/bin:$PATH` in .zshrc to use the bin/doom utility anywhere in terminal
-   Turn on `literate` in init.el in .dooms.d, then create config.org
-   Run `M-x doom/reload` in Emacs after changing the config.org, this will extract the Lisp source codes automatically to config.el
-   Run `doom sync` in terminal every time the package.el is changed
-   How to debug:
    -   turn on `toggle-debug-on-error`, then `doom/reload`
    -   run `doom doctor` in terminal


## Global Backends {#global-backends}


### Doom default {#doom-default}

```emacs-lisp
(package-initialize t)
(setq user-full-name "Eejain Huang"
      user-mail-address "huangyizhen2002@hotmail.com")
```


### Change the default encoding to UTF-8, more suitable for multi-lan environment {#change-the-default-encoding-to-utf-8-more-suitable-for-multi-lan-environment}

```emacs-lisp
(prefer-coding-system       'utf-8)
(set-default-coding-systems 'utf-8)
(set-terminal-coding-system 'utf-8)
(set-keyboard-coding-system 'utf-8)
(setq default-buffer-file-coding-system 'utf-8)
```


### Do not create lockfiles for files being edited {#do-not-create-lockfiles-for-files-being-edited}

references: <https://github.com/Brettm12345/doom-emacs-literate-config/blob/master/config.org#completioncompany>

```emacs-lisp
  (setq create-lockfiles nil)
```


### Company config {#company-config}

reference: <https://github.com/Brettm12345/doom-emacs-literate-config/blob/master/config.org#completioncompany>


#### Set maximum candidates for `company-box` {#set-maximum-candidates-for-company-box}

```emacs-lisp
  (after! company-box
    (setq company-box-max-candidates 5))
```


#### Setup `company-perscient` {#setup-company-perscient}

```emacs-lisp
  (use-package company-prescient
    :after company
    :hook (company-mode . company-prescient-mode))
```


#### Setup company ui {#setup-company-ui}

```emacs-lisp
  (after! company
    (setq company-tooltip-limit 5
          company-tooltip-minimum-width 80
          company-tooltip-minimum 5
          company-backends
          '(company-capf company-dabbrev company-files company-yasnippet)
          company-global-modes '(not comint-mode erc-mode message-mode help-mode gud-mode)))
```


### Ivy config {#ivy-config}

reference: <https://github.com/Brettm12345/doom-emacs-literate-config/blob/master/config.org#completioncompany>


#### Set ripgrep as the default program for ivy project search {#set-ripgrep-as-the-default-program-for-ivy-project-search}

```emacs-lisp
  (setq +ivy-project-search-engines '(rg))
```


#### Setup `ivy-rich` {#setup-ivy-rich}

```emacs-lisp
(after! ivy-rich
  (setq ivy-rich-display-transformers-list
        '(ivy-switch-buffer
          (:columns
           ((ivy-rich-candidate (:width 30 :face bold))
            (ivy-rich-switch-buffer-size (:width 7 :face font-lock-doc-face))
            (ivy-rich-switch-buffer-indicators (:width 4 :face error :align right))
            (ivy-rich-switch-buffer-major-mode (:width 18 :face doom-modeline-buffer-major-mode))
            (ivy-rich-switch-buffer-path (:width 50)))
           :predicate
           (lambda (cand) (get-buffer cand)))
          +ivy/switch-workspace-buffer
          (:columns
           ((ivy-rich-candidate (:width 30 :face bold))
            (ivy-rich-switch-buffer-size (:width 7 :face font-lock-doc-face))
            (ivy-rich-switch-buffer-indicators (:width 4 :face error :align right))
            (ivy-rich-switch-buffer-major-mode (:width 18 :face doom-modeline-buffer-major-mode))
            (ivy-rich-switch-buffer-path (:width 50)))
           :predicate
           (lambda (cand) (get-buffer cand)))
          counsel-M-x
          (:columns
           ((counsel-M-x-transformer (:width 40))
            (ivy-rich-counsel-function-docstring (:face font-lock-doc-face :width 80))))
          counsel-describe-function
          (:columns
           ((counsel-describe-function-transformer (:width 40))
            (ivy-rich-counsel-function-docstring (:face font-lock-doc-face :width 80))))
          counsel-describe-variable
          (:columns
           ((counsel-describe-variable-transformer (:width 40))
            (ivy-rich-counsel-variable-docstring (:face font-lock-doc-face :width 80))))
          counsel-recentf
          (:columns
           ((ivy-rich-candidate (:width 100))
            (ivy-rich-file-last-modified-time (:face font-lock-doc-face)))))))

(after! counsel
  (setq counsel-evil-registers-height 20
        counsel-yank-pop-height 20
        counsel-org-goto-face-style 'org
        counsel-org-headline-display-style 'title
        counsel-org-headline-display-tags t
        counsel-org-headline-display-todo t))
```

```emacs-lisp
  (after! ivy
    (setq ivy-posframe-parameters
          `((min-width . 160)
            (min-height . ,ivy-height)
            (left-fringe . 0)
            (right-fringe . 0)
            (internal-border-width . 10))
          ivy-display-functions-alist
          '((counsel-git-grep)
            (counsel-grep)
            (counsel-pt)
            (counsel-ag)
            (counsel-rg)
            (counsel-notmuch)
            (swiper)
            (counsel-irony . ivy-display-function-overlay)
            (ivy-completion-in-region . ivy-display-function-overlay)
            (t . ivy-posframe-display-at-frame-center))))
(after! ivy
  (setq ivy-use-selectable-prompt t
        ivy-auto-select-single-candidate t
        ivy-rich-parse-remote-buffer nil
        +ivy-buffer-icons nil
        ivy-use-virtual-buffers nil
        ivy-magic-slash-non-match-action 'ivy-magic-slash-non-match-cd-selected
        ivy-height 20
        ivy-rich-switch-buffer-name-max-length 50))
```


#### Add helpful action to `counsel-M-x` {#add-helpful-action-to-counsel-m-x}

```emacs-lisp
  (after! ivy
    (ivy-add-actions
     'counsel-M-x
     `(("h" +ivy/helpful-function "Helpful"))))
```


#### Setup `counsel-tramp` {#setup-counsel-tramp}

```emacs-lisp
  (use-package counsel-tramp
    :commands (counsel-tramp))
```


#### Setup [all-the-icons-ivy](https://github.com/asok/all-the-icons-ivy) {#setup-all-the-icons-ivy}

```emacs-lisp
(use-package all-the-icons-ivy
  :after ivy
  :config
  (dolist (cmd '( counsel-find-file
                  counsel-file-jump
                  projectile-find-file
                  counsel-projectile-find-file
                  counsel-dired-jump counsel-projectile-find-dir
                  counsel-projectile-switch-project))
    (ivy-set-display-transformer cmd #'all-the-icons-ivy-file-transformer)))
```


### Dired config {#dired-config}

reference: <https://github.com/Brettm12345/doom-emacs-literate-config/blob/master/config.org#completioncompany>


#### Set `dired-k` to use human readable styles {#set-dired-k-to-use-human-readable-styles}

```emacs-lisp
  (after! dired-k
    (setq dired-k-human-readable t))
```


#### Set `dired-k` filesize colors {#set-dired-k-filesize-colors}

```emacs-lisp
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


## Global Looks {#global-looks}


### Color scheme and font {#color-scheme-and-font}

```emacs-lisp
(setq doom-font (font-spec :faimily "monospace" :size 14))
(setq doom-theme 'doom-tomorrow-night)
(setq display-line-numbers-style 'relative)
```


### Set up cursor shape in iterm emacs for indicating edit/normal mode {#set-up-cursor-shape-in-iterm-emacs-for-indicating-edit-normal-mode}

the cursor shape of different vim mode is identical by default. use package term-cursor

```emacs-lisp
  (use-package term-cursor)
  (global-term-cursor-mode)
```


### Function to set visual line width {#function-to-set-visual-line-width}

references: <https://www.emacswiki.org/emacs/VisualLineMode>

```emacs-lisp
 (defvar visual-wrap-column nil)
 (defun set-visual-wrap-column (new-wrap-column &optional buffer)
      "Force visual line wrap at NEW-WRAP-COLUMN in BUFFER (defaults
    to current buffer) by setting the right-hand margin on every
    window that displays BUFFER.  A value of NIL or 0 for
    NEW-WRAP-COLUMN disables this behavior."
      (interactive (list (read-number "New visual wrap column, 0 to disable: " (or visual-wrap-column fill-column 0))))
      (if (and (numberp new-wrap-column)
               (zerop new-wrap-column))
        (setq new-wrap-column nil))
      (with-current-buffer (or buffer (current-buffer))
        (visual-line-mode t)
        (set (make-local-variable 'visual-wrap-column) new-wrap-column)
        (add-hook 'window-configuration-change-hook 'update-visual-wrap-column nil t)
        (let ((windows (get-buffer-window-list)))
          (while windows
            (when (window-live-p (car windows))
              (with-selected-window (car windows)
                (update-visual-wrap-column)))
            (setq windows (cdr windows))))))
    (defun update-visual-wrap-column ()
      (if (not visual-wrap-column)
        (set-window-margins nil nil)
        (let* ((current-margins (window-margins))
               (right-margin (or (cdr current-margins) 0))
               (current-width (window-width))
               (current-available (+ current-width right-margin)))
          (if (<= current-available visual-wrap-column)
            (set-window-margins nil (car current-margins))
            (set-window-margins nil (car current-margins)
                                (- current-available visual-wrap-column))))))
```


## Global Editing {#global-editing}


### Key binding for Emacs in iTerm2 environment {#key-binding-for-emacs-in-iterm2-environment}

<https://stackoverflow.com/questions/10660060/how-do-i-bind-c-in-emacs/40222318#40222318>
workflow: in iterm, set preferences-key add a keyboard short cut with emacs key
binding, then choose Send Escape Sequence as the Action, then use the
my/global-map-and-set-key funtion below to bind the short cut to the original
emacs function (find out the function by running describe the key function)
the current key map in iterm is saved in iterm\_emacs.itermkeymap

```emacs-lisp

;; define function
(defun my/global-map-and-set-key (key command &optional prefix suffix)
  "`my/map-key' KEY then `global-set-key' KEY with COMMAND.
 PREFIX or SUFFIX can wrap the key when passing to `global-set-key'."
  (my/map-key key)
  (global-set-key (kbd (concat prefix key suffix)) command))
(defun my/map-key (key)
  "Map KEY from escape sequence \"\e[emacs-KEY\."
  (define-key function-key-map (concat "\e[emacs-" key) (kbd key)))

;; the most important remapping, M-x
(my/global-map-and-set-key "M-x" 'counsel-M-x)

;; comment/uncomment: first bind a new key sequence C-/ (subsitute s-/) for commenting codes, then map iterm key to emacs
(map!
 (:after evil
   :m  "C-/" #'evilnc-comment-or-uncomment-lines))
(my/global-map-and-set-key "C-/" 'evilnc-comment-or-uncomment-lines)

;; copy, cut, paste
(my/global-map-and-set-key "s-x" 'kill-region)
(my/global-map-and-set-key "s-c" 'evil-yank)
(my/global-map-and-set-key "s-v" 'evil-paste-after)

;; create new heading below: first change the default keybinding (s-return) to M-return
(map!
 (:after evil
   :m  "<M-return>" #'+default/newline-below))
(my/global-map-and-set-key "<M-return>" '+default/newline-below)

;; move lines around
(my/global-map-and-set-key "<M-up>" 'drag-stuff-up)
(my/global-map-and-set-key "<M-down>" 'drag-stuff-down)

;; meta drag
(my/global-map-and-set-key "<M-S-up>" 'org-shiftmetaup)
(my/global-map-and-set-key "<M-S-down>" 'org-shiftmetadown)

;; outline promote/demote (metaleft/right)
(my/global-map-and-set-key "M-h" 'org-metaleft)
(my/global-map-and-set-key "M-H" 'org-shiftmetaleft)
(my/global-map-and-set-key "M-l" 'org-metaright)
(my/global-map-and-set-key "M-L" 'org-shiftmetaright)

;; for evaluating r codes, not sure whether works or not yet
(my/global-map-and-set-key "M-d" 'evil-multiedit-match-symbol-and-next)

;; currently I disabled arrow keys for navigation, but command(s) + arrow key still work, and in terminal, use shift + arrow keys for normal arrow key behavior
;; use ctrl + hjkl to navigate in function menus
(my/global-map-and-set-key "<s-left>" 'evil-backward-char)
(my/global-map-and-set-key "<s-right>" 'evil-forward-char)
(my/global-map-and-set-key "<s-down>" 'evil-next-line)
(my/global-map-and-set-key "<s-up>" 'evil-previous-line)


;; create a new delete shortcut for easier access (works in both normal and insert mode)
(map!
 (:after evil
   :m  "s-[" #'evil-delete-backward-char))
(my/global-map-and-set-key "s-[" 'evil-delete-backward-char)

;; for ess-r short cut
(my/global-map-and-set-key "<C-return>" 'ess-eval-line)
(my/global-map-and-set-key "<C-S-return>" 'ess-eval-region-or-function-or-paragraph)
```


### Copy paste from external clipboard (for terminal emacs window) {#copy-paste-from-external-clipboard--for-terminal-emacs-window}

```emacs-lisp
(setq osx-clipboard-mode t)
```


#### <span class="org-todo done ARCV">ARCV</span> auto save and load current session layout (desktop) {#auto-save-and-load-current-session-layout--desktop}

-   State "ARCV"       from "TODO"       <span class="timestamp-wrapper"><span class="timestamp">[2020-06-15 Mon 15:07]</span></span>

<!--listend-->

```emacs-lisp

;; save current window layout and load later: https://emacs.stackexchange.com/questions/2710/switching-between-window-layouts
;; (defvar winstack-stack '()
;;   "A Stack holding window configurations.
;; Use `winstack-push' and
;; `winstack-pop' to modify it.")

;; (defun winstack-push()
;;   "Push the current window configuration onto `winstack-stack'."
;;   (interactive)
;;   (if (and (window-configuration-p (first winstack-stack))
;;          (compare-window-configurations (first winstack-stack) (current-window-configuration)))
;;       (message "Current config already pushed")
;;     (progn (push (current-window-configuration) winstack-stack)
;;            (message (concat "pushed " (number-to-string
;;                                        (length (window-list (selected-frame)))) " frame config")))))

;; (defun winstack-pop()
;;   "Pop the last window configuration off `winstack-stack' and apply it."
;;   (interactive)
;;   (if (first winstack-stack)
;;       (progn (set-window-configuration (pop winstack-stack))
;;              (message "popped"))
;;     (message "End of window stack")))

(use-package psession
  :config
  (psession-mode 1))
```


## Org Mode Backends {#org-mode-backends}


### Org directory {#org-directory}

references: <https://lists.gnu.org/archive/html/emacs-orgmode/2009-10/msg00734.html>

```emacs-lisp
(setq org-directory "~/GoogleDrive/MarkdownNotes/MDNotes")
(setq org-agenda-directory "~/GoogleDrive/MarkdownNotes/MDNotes/")
(setq org-agenda-files (directory-files (expand-file-name org-agenda-directory) t
                                        "^[^\.][^#][[:alnum:]]+\.org$"))
(setq org-default-notes-file (concat org-directory "/notes.org"))
```


### Org keywords and tags {#org-keywords-and-tags}

Previously I cannot override these settings because I changed setting in M-x
cutomization, now I deleted the content in ~/.emacs.d/.local/custom.el and add
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
            (sequence "MARK(m@/!)" "ING(i@/!)" "REPO(r@/!)" "|" "ARCV(a@/!)")
            )))
```


### Org setup {#org-setup}

references: <https://github.com/sk8ingdom/.emacs.d/blob/master/org-mode-config/org-todo-states.el>

```emacs-lisp
  (after! org
    ;; prompt to record time and note when a task is completed
    (setq org-log-done 'note)

    ;; prompt to record time and note when the scheduled date of a task is modified
    (setq org-log-reschedule 'note)

    ;; promopt to record time and note when the deadline of a task is modified
    (setq org-log-redeadline 'note)

    ;; promopt to record time and note when clocking out of a task
    (setq org-log-clock-out 'note)

    (setq org-agenda-todo-ignore-scheduled 'all)

    ;; set the scope of line-editing behavior to the visual line (not actual line)
    (setq vim-style-visual-line-move-text t)
    )

(with-eval-after-load 'org
  (add-to-list 'org-modules 'org-habit t))
```


### Org capture {#org-capture}

references:
<https://emacs.stackexchange.com/questions/19391/can-t-set-directory-for-org-mode-capture>
<https://orgmode.org/manual/Template-expansion.html#Template-expansion>
<https://github.com/sk8ingdom/.emacs.d/blob/master/org-mode-config/org-capture-templates.el>

```emacs-lisp
  (setq org-capture-templates
        '(
          ("t" "Todo" entry (file+headline "~/Googledrive/Markdownnotes/MDNotes/todo.org" "Inbox") "* TODO %?\n  %U\n")
          ("d" "Drafts" entry (file+headline "~/Googledrive/Markdownnotes/MDNotes/todo.org" "Drafts") "* ARCV %?\n  %U\n")
          ("j" "Journal" entry (function org-journal-find-location)
           "* %(format-time-string org-journal-time-format)%^{Title}\n%i%?")
          )
        )
```


### org-journal {#org-journal}

references: <https://github.com/bastibe/org-journal>

```emacs-lisp
(customize-set-variable 'org-journal-dir "~/GoogleDrive/Markdownnotes/MDNotes/")
(customize-set-variable 'org-journal-date-format "%A, %d %B %Y")
(customize-set-variable 'org-journal-file-format "%Y%m%d.org")
(require 'org-journal)
  ;; (use-package org-journal
  ;;   :ensure t
  ;;   :defer t
  ;;   :custom
  ;;   (org-journal-dir "~/GoogleDrive/MarkdownNotes/MDNotes/")
  ;;   (org-journal-file-format "%Y%m%d.org")
  ;;   (org-journal-date-format "%A, %B %d %Y"))
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
          (org. t)
          (python. t)
          (latex. t)
          (R. t)))

;; activate Babel languages
(org-babel-do-load-languages
 'org-babel-load-languages
 org-babel-load-languages)

  (add-hook 'org-babel-after-execute-hook 'org-display-inline-images)
  (add-hook 'org-mode-hook 'org-display-inline-images)
```


## Org Mode Looks {#org-mode-looks}


### Change the character that displays on collapsed headings {#change-the-character-that-displays-on-collapsed-headings}

```emacs-lisp
  (setq org-ellipsis " ▼ ")
```


### Line wrapping {#line-wrapping}

```emacs-lisp
;; visual line mode will wrap lines at the window border without actually insert line breaks
(add-hook 'org-mode-hook #'visual-line-mode)

;; turn off auto fill mode so there won't be any hard line breaks after the wrap column
(add-hook 'org-mode-hook #'turn-off-auto-fill)

;; also add (setq evil-respect-visual-line-mode t) to init.el, enable navigation in visual line mode (evel-next-visual-line)
(add-hook 'org-mode-hook #'adaptive-wrap-prefix-mode)
```


## Org Mode Editing {#org-mode-editing}


### Key binding for showing all todo headings {#key-binding-for-showing-all-todo-headings}

```emacs-lisp
  (global-set-key (kbd "C-c t") 'org-show-todo-tree)
```


### Key binding for outline quicklook/goto {#key-binding-for-outline-quicklook-goto}

```emacs-lisp
  (global-set-key (kbd "C-c r") 'counsel-outline)
```


### Key binding for jumping to the end of the line {#key-binding-for-jumping-to-the-end-of-the-line}

```emacs-lisp
  (map!
   (:after evil
     :m  "C-e" #'doom/forward-to-last-non-comment-or-eol))
```


### Key binding to insert newlines above and below {#key-binding-to-insert-newlines-above-and-below}

Similar to [vim-unimpaired](https://github.com/tpope/vim-unimpaired)

```emacs-lisp
  (map!
   (:after evil
     :m  "] SPC" #'evil-motion-insert-newline-below
     :m  "[ SPC" #'evil-motion-insert-newline-above))
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
useful functions: R (run-ess-r), popup/raise, ess-eval-region-or-function-or-paragraph-and-step

```emacs-lisp
  (with-eval-after-load "ess"
    (add-hook 'ess-r-mode-hook
              (lambda()
                'eglot-ensure
                (make-local-variable 'company-backends)
                (delete-dups (push 'company-capf company-backends))
                (delete-dups (push 'company-files company-backends))))
    (require 'ess-mode)

    ;; evaluate codes with ctrl + enter (by line) or ctrl + shift + enter (by paragraph)
    (define-key ess-mode-map (kbd "<C-return>") 'ess-eval-line)
    (define-key ess-mode-map (kbd "<C-S-return>") 'ess-eval-region-or-function-or-paragraph)
    ;; define the same keybindings for normal state (otheriwse the keybinding will only work in edit state)
    (with-eval-after-load 'evil-maps
      (define-key evil-normal-state-map (kbd "<C-return>") 'ess-eval-line))
    (with-eval-after-load 'evil-maps
      (define-key evil-normal-state-map (kbd "<C-S-return>") 'ess-eval-region-or-function-or-paragraph))


    ;; Set ESS options
    (setq
     ess-auto-width 'window
     ess-use-auto-complete nil
     ess-use-company 't
     ;; ess-r-package-auto-set-evaluation-env nil
     inferior-ess-same-window nil
   ess-indent-with-fancy-comments nil ; don't indent comments
     ess-eval-visibly t                 ; enable echoing input
     ess-eval-empty t                   ; don't skip non-code lines.
     ess-ask-for-ess-directory nil ; start R in the working directory by default
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
       (ess-R-fl-keyword:F&T)))))
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


## Appendix {#appendix}

Other private configurations that are not yet in literate format.


### init.el {#init-dot-el}

```emacs-lisp
(doom! :input

       :completion
       (company +auto +childframe)
       (ivy +fuzzy +childframe)

       :ui
       (popup +all +defaults)
       doom
       doom-dashboard
       doom-quit
       modeline
       ophints
       fill-column
       hl-todo
       nav-flash
       neotree
       vc-gutter
       vi-tilde-fringe
       window-select

       :editor
       (evil +everywhere)
       file-templates
       format
       lispy
       multiple-cursors
       rotate-text
       snippets
       fold

       :emacs
       (dired +icons)
       electric
       vc

       :term
       eshell
       term

       :checkers
       spell
       syntax

       :tools
       eval
       (lookup
        +devdocs
        +docsets)
       gist
       make
       magit
       pass
       rgb
       tmux
       upload

       :lang
       data              ; config/data formats
       emacs-lisp        ; drown in parentheses
       ess               ; emacs speaks statistics
       latex             ; writing papers in Emacs has never been so fun
       markdown          ; writing docs for people to ignore
       (org              ; organize your plain life in plain text
        +dragndrop       ; drag & drop files/images into org buffers
        +hugo            ; use Emacs for hugo blogging
        +pandoc          ; export-with-pandoc support
        +attach
        +babel
        +capture
        +export
        +present)        ; using org-mode for presentations

       :config
       (default +bindings +smartparens)
       literate)

(setq evil-respect-visual-line-mode t)
```


### pacakges.el {#pacakges-dot-el}

```emacs-lisp
(package! term-cursor
  :recipe (:host github :repo "h0d/term-cursor.el"))

(package! osx-clipboard
  :recipe (:host github :repo "joddie/osx-clipboard-mode"))

(package! org-journal)
(package! adaptive-wrap)
(package! ox-hugo)
(package! polymode)
(package! poly-markdown)
(package! poly-R)
(package! poly-org)
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

;; ui/modeline
(package! doom-modeline)
(package! anzu)
(package! evil-anzu)
```


## Source Files {#source-files}

-   [init.el](/static/files/init.el)
-   [pakages.el](/static/files/pakages.el)
-   [config.org](/static/files/config.org)
-   [iTerm2 keymapping](/static/files/EmacsMain.itermkeymap)
