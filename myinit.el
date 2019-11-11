;;; myinit.el --- Config file for my emacs
;;; Commentary:
;;; My config file, all the packages I'm using, except use-package.

;;; Code:
;; use-package-setting:

(setq use-package-always-ensure t)
(setq use-package-verbose t)
;; suppress warning
(setq ad-redefinition-action 'accept)

;; interface tweaks
(setq inhibit-startup-message t)
(tool-bar-mode -1)
(scroll-bar-mode -1)
(blink-cursor-mode -1)
(fset 'yes-or-no-p 'y-or-n-p)
(global-set-key (kbd "<f5>") 'revert-buffer)
;; ns-popup-font-panel was bound to s-t.
(global-unset-key (kbd "s-t"))

;; Keymaps

(define-key input-decode-map [?\C-m] [C-m])

(eval-and-compile
  (mapc #'(lambda (entry)
            (define-prefix-command (cdr entry))
            (bind-key (car entry) (cdr entry)))
        '(("C-,"   . my-ctrl-comma-map)
          ("<C-m>" . my-ctrl-m-map)

          ("C-h e" . my-ctrl-h-e-map)
          ("C-h x" . my-ctrl-h-x-map)

          ("C-c b" . my-ctrl-c-b-map)
          ("C-c e" . my-ctrl-c-e-map)
          ("C-c m" . my-ctrl-c-m-map)
          ("C-c w" . my-ctrl-c-w-map)
          ("C-c y" . my-ctrl-c-y-map)
          ("C-c H" . my-ctrl-c-H-map)
          ("C-c N" . my-ctrl-c-N-map)
          ("C-c (" . my-ctrl-c-open-paren-map)
          ("C-c -" . my-ctrl-c-minus-map)
          ("C-c =" . my-ctrl-c-equals-map)
          ("C-c ." . my-ctrl-c-r-map)
          )))


;; Opacity
(defun sanityinc/adjust-opacity (frame incr)
  "Adjust-opacity copied from SachaChua.works on the current FRAME, INCR by 2/-1."
    (let* ((oldalpha (or (frame-parameter frame 'alpha) 100))
	   (newalpha (+ incr oldalpha)))
      (when (and (<= frame-alpha-lower-limit newalpha) (>= 100 newalpha))
	(modify-frame-parameters frame (list (cons 'alpha newalpha))))))

  (global-set-key
   (kbd "M-C-8")
   (lambda () (interactive) (sanityinc/adjust-opacity nil -2)))
  (global-set-key
   (kbd "M-C-9")
   (lambda () (interactive) (sanityinc/adjust-opacity nil 2)))
  (global-set-key
   (kbd "M-C-0")
   (lambda () (interactive) (modify-frame-parameters nil `((alpha . 100)))))

(setq frame-title-format
  '(:eval
    (if buffer-file-name
	(replace-regexp-in-string
	 "\\\\" "/"
	 (replace-regexp-in-string
	  (regexp-quote (getenv "HOME")) "~"
	  (convert-standard-filename buffer-file-name)))
      (buffer-name))))


;; (defun er-open-with (arg)
;;   "Open visited file in default external program.

;; With a prefix ARG always prompt for command to use."
;;   (interactive "P")
;;   (when buffer-file-name
;;     (shell-command (concat
;; 		    (cond
;; 		     ((and (not arg) (eq system-type 'darwin)) "open")
;; 		     ((and (not arg) (member system-type '(gnu gnu/linux gnu/kfreebsd))) "xdg-open")
;; 		     (t (read-shell-command "Open current file with: ")))
;; 		    " "
;; 		    (shell-quote-argument buffer-file-name)))))
;; (global-set-key (kbd "C-c o") #'er-open-with)

;;; Libraries
(use-package diminish)


(use-package try
  :defer 10)

(use-package which-key
  :diminish (which-key-mode)
  :defer 3
  :config
  (which-key-mode))

(use-package eldoc
  :diminish)

;; org-mode
(use-package org-bullets
  :defer t
  :bind (("C-c l" . org-store-link)
	 ("C-c a" . org-agenda)
	 ("C-c c" . org-iswitchb))
  :hook (org-mode . org-bullets-mode))

(require 'org)
(setq-default major-mode 'org-mode)
;;steal from hrs
(setq org-directory "~/Dropbox/notes")

(setq org-default-notes-file (concat org-directory "/notes.org"))

(defun org-file-path (filename)
  "Return the absolute address of an org file, given its relative name FILENAME."
  (concat (file-name-as-directory org-directory) filename))

(setq org-index-file (org-file-path "index.org"))
(setq org-default-notes-file org-index-file)
(setq org-archive-location
      (concat (org-file-path "archive.org") "::* From %s"))

(setq org-agenda-files (list org-index-file))

(defun hrs/mark-done-and-archive ()
  "Mark the state of an org-mode item as DONE and archive it."
  (interactive)
  (org-todo 'done)
  (org-archive-subtree))

(define-key org-mode-map (kbd "C-c C-x C-s") 'hrs/mark-done-and-archive)

(setq org-log-done 'time)
;;(setq org-agenda-files (list "~/Dropbox/notes/schedule.org"))

(add-to-list 'org-structure-template-alist
	     '("el" "#+BEGIN_SRC emacs-lisp\n?\n#+END_SRC"))
(add-hook 'org-mode-hook
	  '(lambda ()
	     (visual-line-mode 1)))
(setq org-hide-emphasis-markers t)
(require 'cl)				;for delete*
(setq org-emphasis-alist
      (cons '("+" (:strike-through t :foreground "gray"))
	    (delete* "+" org-emphasis-alist :key 'car :test 'equal)))
(setq org-emphasis-alist
      (cons '("*" (bold :foreground "red"))
	    (delete* "*" org-emphasis-alist :key 'car :test 'equal)))
;; 使得中英文表格对其, 需要先安装https://www.google.co.kr/get/noto/
;; (set-face-attribute 'org-table nil :family "Noto Sans Mono CJk SC")

;; for python
(org-babel-do-load-languages
 'org-babel-load-languages
 '((python . t)
   (perl . t)
   (emacs-lisp . t)
   (shell . t)))
;; Refiling according to the document’s hierarchy.
(setq org-refile-use-outline-path t)
(setq org-outline-path-complete-in-steps nil)

;; Hit C-c i to quickly open up my todo list.
(defun my/open-index-file ()
  "Open the master org TODO list."
  (interactive)
  (find-file org-index-file)
  (flycheck-mode -1)
  (end-of-buffer))

(global-set-key (kbd "C-c i") 'my/open-index-file)
;; ox-*
(use-package ox-twbs
  :after org-mode)


(use-package org-pdfview
  :defer 4)

;; ;; ido-mode
;; (setq ido-enable-flex-matching t)
;; (setq ido-everywhere t)
;; (ido-mode 1)

;; ;;(defalias 'list-buffers 'ibuffer)
;; (defalias 'list-buffers 'ibuffer-other-window)

;; backup copy from SachaChua
(setq backup-directory-alist '(("." . "~/.emacs.d/backups")))

(setq delete-old-versions -1)
(setq version-control t)
(setq vc-make-backup-files t)
(setq auto-save-file-name-transforms '((".*" "~/.emacs.d/auto-save-list/" t)))

(use-package ace-window
  :init (setq aw-swap-invert t)
  :bind* (("C-<return>" . ace-window)
          ("M-o" . ace-swap-window)))

(use-package winner
  :config
  (winner-mode))

;; based on http://tuhdo.github.io/helm-intro.html
(use-package helm
  :diminish (helm-mode)
  :bind (("M-x" . helm-M-x)
         ("M-y" . helm-show-kill-ring)
         ("C-c h" . helm-command-prefix)
         ("C-h SPC" . helm-all-mark-rings)
         ("C-x r b" . helm-filtered-bookmarks)
         ("C-x C-f" . helm-find-files)
         ("C-x b" . helm-mini)
         :map helm-map
         ;; rebind tab to run persistent action
         ("<tab>" . helm-execute-persistent-action)
         ;; make Tab work in terminal
         ("C-i" . helm-execute-persistent-action)
         ("C-z" . helm-select-action))
  :config
  (require 'helm-config)
  (global-unset-key (kbd "C-x c"))
  (global-set-key (kbd "C-c h o") 'helm-occur)
  (when (executable-find "curl")
    (setq helm-google-suggest-use-curl-p t))
  (when (string= system-type "darwin")       
    ;; brew info findutils
    (setq helm-locate-command
          "glocate %s %s"
          helm-locate-create-db-command
          "gupdatedb --output='%s' --localpaths='%s'"))

  (setq helm-split-window-inside-p t ; open helm buffer inside current window, not occupy whole other window
        ; helm-move-to-line-cycle-in-source t
        helm-ff-search-library-in-sexp t ; search for library in `require' and `declare-function' sexp
        ;;helm-scroll-amount 8 ; scroll 8 lines other window using M-<next>/M-<prior>
        helm-ff-file-name-history-use-recentf t
        helm-echo-input-in-header-line t
        ;; helm-autoresize-max-height 0
        ;; helm-autoresize-min-height 20
        helm-M-x-fuzzy-match t
        helm-dwim-target 'next-window
        helm-ff-auto-update-initial-value 1)
        ;; (helm-autoresize-mode 1)

  (custom-set-faces
   '(helm-selection ((t (:background "systemPurpleColor" :foreground "white")))))
  (helm-mode 1))

(use-package helm-ls-git
  :after
  (helm-mode)
  :config
  (global-set-key (kbd "C-x C-d") 'helm-browse-project))

(use-package helm-dash
  ;;  fixed dash-doc.el temporary-file-directory to /tmp/ for Catalina
  :commands helm-dash
  :config
  (setq helm-dash-enable-debugging t)
  (setq helm-dash-browser-func (quote eww))
  (setq helm-dash-docsets-path "/Users/zhaoweipu/Library/Application Support/Dash/DocSets/")
  (add-to-list 'helm-dash-common-docsets "Go")  
  (add-to-list 'helm-dash-common-docsets "Django")
  (add-to-list 'helm-dash-common-docsets "Python 2")
  (add-to-list 'helm-dash-common-docsets "Python 3")
  (add-to-list 'helm-dash-common-docsets "Python 3")
  (add-to-list 'helm-dash-common-docsets "Redis"))



(use-package dash-at-point
  :bind ("C-c D" . dash-at-point))
  ;; (add-to-list 'dash-at-point-mode-alist
  ;;              '(python-mode . "python")))


(use-package projectile
  :bind-keymap
  ("C-c p" . projectile-command-map)
  ("s-p" . projectile-command-map)
  :config
  (use-package helm-projectile
    :ensure t
    :config
    (use-package helm-ag
      :config
      (setq helm-ag-insert-at-point 'symbol))
    (helm-projectile-on))
  (setq projectile-completion-system 'helm)
  (setq projectile-switch-project-action 'helm-projectile)
  ;; (setq projectile-enable-caching t)
  (projectile-mode +1))


(use-package helm-descbinds
  :defer 7
  :config
  (helm-descbinds-mode))

(use-package command-log-mode
  :bind (("C-c e M" . command-log-mode)
         ("C-c e L" . clm/open-command-log-buffer))
  :config (setq clm/log-command-exceptions* nil))


(use-package ivy
  :disabled t
  :diminish (ivy-mode)
  :config
  (setq ivy-count-format "%d/%d "))

(use-package counsel
  :disabled t
  :after ivy
  :bind
  ("M-y" . counsel-yank-pop))


;; Swiper
(use-package swiper
  :disabled t
  :bind
   ("C-s" . swiper)
   ("C-r" . swiper)
   ;; ("C-c C-r" . ivy-resume)
   ("M-x" . counsel-M-x)
   ("C-x C-f" . counsel-find-file)
   ("C-x b" . ivy-switch-buffer)
   ("C-c g" . counsel-git)
   ("C-c j" . counsel-git-grep)
   ("C-c k" . counsel-ag)
   ("C-x l" . counsel-locate)
   ("C-S-o" . counsel-rhythmbox)
  :config
  (ivy-mode 1)
  (setq ivy-use-virtual-buffers t)
  ;; (setq ivy-display-style 'fancy)
  (define-key read-expression-map (kbd "C-r") 'counsel-expression-history))

;; Avy - navigate by searching for a letter on the screen and jumping to it
(use-package avy
  :bind* ("C-." . avy-goto-char-timer)
  :config
  (avy-setup-default))

(use-package avy-zap
  :bind
  ("M-z" . avy-zap-to-char-dwim)
  ("M-Z" . avy-zap-up-to-char-dwim))

(use-package phi-search
  :defer 5)

(use-package phi-search-mc
  :after (phi-search multiple-cursors)
  :config
  (phi-search-mc/setup-keys)
  (add-hook 'isearch-mode-mode #'phi-search-from-isearch-mc/setup-keys))


(use-package selected
  :demand t
  :diminish selected-minor-mode
  :bind (:map selected-keymap
              ("[" . align-code)
              ("f" . fill-region)
              ("U" . unfill-region)
              ("d" . downcase-region)
              ("u" . upcase-region)
              ("r" . reverse-region)
              ("s" . sort-lines))
  :config
  (selected-global-mode 1))


;; (use-package mule
;;   :no-require t
;;   :config
;;   (prefer-coding-system 'utf-8)
;;   (set-terminal-coding-system 'utf-8)
;;   (setq x-select-request-type '(UTF8_STRING COMPOUND_TEXT TEXT STRING)))

;; (use-package multi-term
;;   :bind (("C-c t" . multi-term-next)
;;          ("C-c T" . multi-term))
;;   :init
;;   (defun screen ()
;;     (interactive)
;;     (let (term-buffer)
;;       ;; Set buffer.
;;       (setq term-buffer
;;             (let ((multi-term-program (executable-find "screen"))
;;                   (multi-term-program-switches "-DR"))
;;               (multi-term-get-buffer)))
;;       (set-buffer term-buffer)
;;       (multi-term-internal)
;;       (switch-to-buffer term-buffer)))

;;   :config
;;   (require 'term)

;;   (defalias 'my-term-send-raw-at-prompt 'term-send-raw)

  ;; (defun my-term-end-of-buffer ()
  ;;   (interactive)
  ;;   (call-interactively #'end-of-buffer)
  ;;   (if (and (eobp) (bolp))
  ;;       (delete-char -1)))

  ;; (defadvice term-process-pager (after term-process-rebind-keys activate)
  ;;   (define-key term-pager-break-map  "\177" 'term-pager-back-page)))

(use-package multiple-cursors
  :after (phi-search selected)
  :defer 5

  ;; - Sometimes you end up with cursors outside of your view. You can scroll
  ;;   the screen to center on each cursor with `C-v` and `M-v`.
  ;;
  ;; - If you get out of multiple-cursors-mode and yank - it will yank only
  ;;   from the kill-ring of main cursor. To yank from the kill-rings of every
  ;;   cursor use yank-rectangle, normally found at C-x r y.

  :bind (
         ;; Mark one more occurrence
         ("<C-m> w"     . mc/mark-next-like-this-word)
         ("<C-m> y"     . mc/mark-next-like-this-symbol)          
         ("<C-m> x"     . mc/mark-more-like-this-extended)
         ;; Mark many occurrences
         ("<C-m> c"     . mc/mark-all-dwim)
         ("<C-m> ^"     . mc/edit-beginnings-of-lines)
         ("<C-m> `"     . mc/edit-beginnings-of-lines)
         ("<C-m> $"     . mc/edit-ends-of-lines)
         ("<C-m> '"     . mc/edit-ends-of-lines)
         ("<C-m> W"     . mc/mark-all-words-like-this)
         ("<C-m> Y"     . mc/mark-all-symbols-like-this)
         ("<C-m> a"     . mc/mark-all-like-this-dwim)
         ("<C-m> r"     . mc/mark-all-in-region)
         ("<C-m> %"     . mc/mark-all-in-region-regexp)
         ("<C-m> t"     . mc/mark-sgml-tag-pair)
         ("<C-m> C-SPC" . mc/mark-pop)
         ("<C-m> ("     . mc/mark-all-symbols-like-this-in-defun)
         ("<C-m> C-("   . mc/mark-all-words-like-this-in-defun)
         ("<C-m> M-("   . mc/mark-all-like-this-in-defun)
         ("<C-m> ["     . mc/vertical-align-with-space)
         ("<C-m> {"     . mc/vertical-align)
         ("S-<down-mouse-1>")
         ("S-<mouse-1>" . mc/add-cursor-on-click)
         ;; Special
         ("<C-m> s"     . set-rectangular-region-anchor)
         ("<C-m> R"     . mc/reverse-regions)
         ("<C-m> S"     . mc/sort-regions)
         ("<C-m> l"     . mc/insert-letters)
         ("<C-m> n"     . mc/insert-numbers)
         ("<C-m> C-x"   . reactivate-mark))
 
  :bind (:map selected-keymap
              ("c"   . mc/edit-lines)
              ("."   . mc/mark-next-like-this)
              ("<"   . mc/unmark-next-like-this)
              ("C->" . mc/skip-to-next-like-this)
              (","   . mc/mark-previous-like-this)
              (">"   . mc/unmark-previous-like-this)
              ("C-<" . mc/skip-to-previous-like-this)
              ("y"   . mc/mark-next-symbol-like-this)
              ("Y"   . mc/mark-previous-symbol-like-this)
              ("w"   . mc/mark-next-word-like-this)
              ("W"   . mc/mark-previous-word-like-this))

  :preface
  (defun reactivate-mark ()
    (interactive)
    (activate-mark)))

(use-package ace-mc
  :bind (("<C-m> h"   . ace-mc-add-multiple-cursors)
         ("<C-m> M-h" . ace-mc-add-single-cursor)))

(use-package company
    :diminish (company-mode)
  :hook (prog-mode . company-mode)
  :config
  (setq company-idle-delay 0))

(use-package color-theme-modern
  :disabled t)

(use-package color-theme-sanityinc-tomorrow)

(use-package doom-themes
  :disabled t)

(use-package solarized-theme
  :disabled t)

(load-theme 'sanityinc-tomorrow-bright t)

(use-package ox-reveal
      :defer 5
      :load-path "~/workspace/git/org-reveal")
;;      :hook org-mode)

;;(setq org-reveal-root "http://cdn.jsdelivr.net/reveal.js/3.0.0/")
(setq org-reveal-root "https://cdn.jsdelivr.net/npm/reveal.js@3.8.0/")
;;(setq org-reveal-root "file:/Users/zhaoweipu/workspace/git/reveal.js/")
(setq org-reveal-mathjax t)

(use-package htmlize)

(use-package flycheck
  :hook
  (prog-mode . flycheck-mode))

(use-package flycheck-color-mode-line
  :hook (flycheck-mode . flycheck-color-mode-line-mode))

(use-package smart-mode-line
  :disabled t
  :config
  ;; See https://github.com/Malabarba/smart-mode-line/issues/217
  (setq mode-line-format (delq 'mode-line-position mode-line-format))
  (sml/setup)

  (sml/apply-theme 'dark)
  (remove-hook 'display-time-hook 'sml/propertize-time-string))

;; (use-package linum-mode
;;   :hook
;;   (prog-mode))
(add-hook 'prog-mode-hook 'linum-mode)

;; The package is "python" but the mode is "python-mode":
(use-package python
  :mode ("\\.py\\'" . python-mode)
  :interpreter ("python" . python-mode))

(setq python-indent-guess-indent-offset t)
(setq python-indent-guess-indent-offset-verbose nil)

(use-package pyvenv
  :hook (python-mode . pyvenv-mode)
  :config
  (setenv "WORKON_HOME" "/usr/local/anaconda3/envs")
  (pyvenv-workon "py3"))

(use-package elpy
  :disabled t
  :defer t
  :hook
  (python-mode . elpy-mode)
  :config
  (setq eldoc-idle-delay 1)
  (when (require 'flycheck nil t)
    (setq elpy-modules (delq 'elpy-module-flymake elpy-modules)))

    ;; force it to use balck, as there this function in elpy.el seems
    ;; can't find black
  (defun elpy-format-code ()
    "Format code using the available formatter."
    (interactive)
    (elpy-black-fix-code)))

;; as C-c C-o is so handy in elpy, I'll keep it with anaconda-mode
(defun elpy-occur-definitions ()
  "Display an occur buffer of all definitions in the current buffer.
Also, switch to that buffer."
  (interactive)
  (let ((list-matching-lines-face nil))
    (occur "^\s*\\(\\(async\s\\|\\)def\\|class\\)\s"))
  (let ((window (get-buffer-window "*Occur*")))
    (if window
        (select-window window)
      (switch-to-buffer "*Occur*"))))

(use-package anaconda-mode
  :commands anaconda-mode
  :ensure t
  :init
  (add-hook 'python-mode-hook 'anaconda-mode)
  (add-hook 'python-mode-hook 'anaconda-eldoc-mode)
  :config
  (define-key anaconda-mode-map  (kbd "M-/") 'anaconda-mode-show-doc)
  (define-key anaconda-mode-map  (kbd "M-.") 'anaconda-mode-find-definitions)
  (define-key anaconda-mode-map  (kbd "M-,") 'pop-tag-mark)
  (define-key anaconda-mode-map  (kbd "M-r") nil)
  (define-key anaconda-mode-map  (kbd "C-c C-o") 'elpy-occur-definitions)
  (setq anaconda-mode-localhost-address "localhost"))

(use-package company-anaconda
  :after (company anaconda-mode))

(eval-after-load "company"
  '(add-to-list 'company-backends 'company-anaconda))

;;black
(use-package blacken
  :ensure t
  :bind ("C-c b" . blacken-buffer))

;; https://github.com/proofit404/anaconda-mode/issues/255
;; (setq url-proxy-services
;;       '(("no_proxy" . "^\\(127.0.0.1\\|localhost\\|10.*\\)")
;;         ("http" . "127.0.0.1:6152")
;;         ("https" . "127.0.0.1:6152")))

(use-package web-mode
  :defer 5
  :config
  (add-to-list 'auto-mode-alist '("\\.html?\\'" . web-mode))
  (add-to-list 'auto-mode-alist '("\\.vue?\\'" . web-mode))
  (add-to-list 'auto-mode-alist '("\\.phtml\\'" . web-mode))
  (add-to-list 'auto-mode-alist '("\\.tpl\\.php\\'" . web-mode))
  (add-to-list 'auto-mode-alist '("\\.[agj]sp\\'" . web-mode))
  (add-to-list 'auto-mode-alist '("\\.as[cp]x\\'" . web-mode))
  (add-to-list 'auto-mode-alist '("\\.erb\\'" . web-mode))
  (add-to-list 'auto-mode-alist '("\\.mustache\\'" . web-mode))
  (add-to-list 'auto-mode-alist '("\\.djhtml\\'" . web-mode))
  (setq web-mode-engines-alist
        '(("django"    . "\\.html\\'")))
  (setq web-mode-ac-sources-alist
        '(("css" . (ac-source-css-property))
          ("vue" . (ac-source-words-in-buffer ac-source-abbrev))
          ("html" . (ac-source-words-in-buffer ac-source-abbrev))))
  (setq-local standard-indent 2)
  (setq web-mode-enable-current-element-highlight t)
  (setq web-mode-enable-current-column-highlight t)
  (setq web-mode-enable-auto-closing t)
  (setq web-mode-enable-auto-quoting t)) ;; this fixes the quote problem I mentioned

(use-package php-mode
  :mode ("\\.php\\'" . php-mode)
  :interpreter ("php" . python-mode))

(use-package lua-mode
  :mode "\\.lua\\'"
  :interpreter "lua"
  :config
  (setq lua-indent-level 4))

(use-package go-mode
  :mode ("\\.go\\'" . go-mode))



(use-package nginx-mode
  :commands nginx-mode)

(use-package yasnippet
  :defer 3
  :diminish (yas-minor-mode)
  ;;:after flycheck
  :init
  (yas-global-mode 1))

(use-package yasnippet-snippets)

(use-package expand-region
  :bind ("C-=" . er/expand-region))

(use-package magit
  :bind
  ("C-x g" . magit-status)
  ("C-x M-g" . magit-dispatch))

(use-package git-gutter
  :defer 1
  :diminish git-gutter-mode
  :init
  (global-git-gutter-mode +1))

(use-package regex-tool
  :commands regex-tool)

(defun regex-tool-perl ()
  "Set perl as backend and run regex-tool."
  (interactive)
  (setq regex-tool-backend 'perl)
  (regex-tool))
  
(defun regex-tool-emacs ()
  "Set perl as backend and run regex-tool."
  (interactive)
  (setq regex-tool-backend 'emacs)
  (regex-tool))

;; use C-x u to see the visual undo tree
;; use C-x p / n / f b
;; q to quit the undo tree visualizer
(use-package undo-tree
  :disabled t
  :diminish (undo-tree-mode)
  :init
  (global-undo-tree-mode))

(setq-default indent-tabs-mode nil)

;; (setq enable-recursive-minibuffers t)

;;  (use-package smart-mode-line
;;    :init
;; ;;   (setq sml/override-theme nil)
;;    (setq sml/no-confirm-load-theme t)
;;    :config
;;     (sml/setup))

;; (use-package hungry-delete
;;   :config
;;   (global-hungry-delete-mode))

(use-package expand-region
  :bind ("C-=" . er/expand-region))

(use-package iedit
  :disabled t)

(defun narrow-or-widen-dwim (p)
  "Widen if buffer is narrowed, narrow-dwim otherwise.

Dwim means: region, org-src-block, org-subtree, or
defun, whichever applies first.  Narrowing to
org-src-block actually calls `org-edit-src-code'.

With prefix P, don't widen, just narrow even if buffer
is already narrowed."
  (interactive "P")
  (declare (interactive-only))
  (declare-function LaTeX-narrow-to-environment "tex-mode")
  (cond ((and (buffer-narrowed-p) (not p)) (widen))
	((region-active-p)
	 (narrow-to-region (region-beginning)
			   (region-end)))
	((derived-mode-p 'org-mode)
	 ;; `org-edit-src-code' is not a real narrowing
	 ;; command. Remove this first conditional if
	 ;; you don't want it.
	 (cond ((ignore-errors (org-edit-src-code) t)
		(delete-other-windows))
	       ((ignore-errors (org-narrow-to-block) t))
	       (t (org-narrow-to-subtree))))
	((derived-mode-p 'latex-mode)
	 (LaTeX-narrow-to-environment))
	(t (narrow-to-defun))))

;; (define-key endless/toggle-map "n #'narrow-or-widen-dwim)
;; This line actually replaces Emacs' entire narrowing
;; keymap, that's how much I like this command. Only
;; copy it if that's what you want.
(define-key ctl-x-map "n" #'narrow-or-widen-dwim)
;; (add-hook 'LaTeX-mode-hook
;; 	  (lambda ()
;; 	    (define-key LaTeX-mode-map "\C-xn"
;; 	      nil)))
(eval-after-load 'org-src
  '(define-key org-src-mode-map
     "\C-x\C-s" #'org-edit-src-exit))


;; [[http://pragmaticemacs.com/emacs/add-the-system-clipboard-to-the-emacs-kill-ring/][ADD THE SYSTEM CLIPBOARD TO THE EMACS KILL-RING]]
(setq save-interprogram-paste-before-kill t)


;; (load "~/Dropbox/mu4econfig.el" t)

(use-package deft
  :bind ("C-c d" . deft)
  :commands (deft)
  :config
  (setq deft-directory "~/Dropbox/notes"
	deft-extensions '("org")
	deft-default-extension "org"
	deft-use-filename-as-title t
	deft-use-filter-string-for-filename t))


(setenv "PKG_CONFIG_PATH" "/usr/local/lib/pkgconfig:/usr/local/Cellar/libffi/3.2.1/lib/pkgconfig")
(use-package pdf-tools
  :magic ("%PDF" . pdf-view-mode)
  :config
  (pdf-tools-install :no-query))

;; (use-package pdf-tools
;;   :config
;;   (pdf-loader-install))


;; sdcv
(use-package showtip
  :ensure t)
(use-package popup
  :ensure t)
(use-package pos-tip
  :ensure t)

(use-package sdcv
  :load-path "~/workspace/git/sdcv.el"
  :demand t
  :config
  (global-set-key (kbd "C-x t") 'sdcv-search-pointer))

(use-package wsd-mode
  :defer 3
  :config
  (setq wsd-style "napkin"))

(delete-file "~/Library/Colors/Emacs.clr")

(when (string= system-type "darwin")       
  (setq dired-use-ls-dired nil))

(define-key global-map (kbd "C-z") nil)


(use-package leetcode
  :bind
  ("C-x l" . leetcode)
  :config
  (setq leetcode-prefer-language "python3"))
  
(use-package exec-path-from-shell
  :config (dolist (var '("SSH_AUTH_SOCK" "SSH_AGENT_PID" "GPG_AGENT_INFO" "LANG" "LC_CTYPE"))
            (add-to-list 'exec-path-from-shell-variables var)))


;; (when (or (memq window-system '(mac ns x))
;;           (unless (memq system-type '(ms-dos windows-nt))
;;             (daemonp)))
;;   (exec-path-from-shell-initialize))

(when (string= system-type "darwin")
  ;;delete on macos
  (setq delete-by-moving-to-trash t)
  (setq trash-directory "~/.Trash")
  (exec-path-from-shell-initialize))


;; (use-package personal
;; ;;  :load-path "~/.emacs.d"
;;   :bind (("M-A m" . my-open-Messages)
;;          ("M-A t" . my-open-Things3)
;;          ("M-A w" . my-open-WeChat)
;;          ("M-A s" . my-open-Safari)
;;          ("M-A f" . my-open-Finder)))


(use-package tramp
  :defer 5
  :config
  ;; jww (2018-02-20): Without this change, tramp ends up sending hundreds of
  ;; shell commands to the remote side to ask what the temporary directory is.
  (put 'temporary-file-directory 'standard-value '("/tmp"))
  (setq tramp-auto-save-directory "~/.cache/emacs/backups"
        tramp-persistency-file-name "~/.emacs.d/data/tramp"))

(use-package tramp-sh
  :load-path "lisp")

(use-package link-hint
  :defer 10
  :bind ("C-c C-o" . link-hint-open-link)
  :config
  (add-hook 'eww-mode-hook
            #'(lambda () (bind-key "f" #'link-hint-open-link eww-mode-map))))
  ;; :config
  ;; (add-to-list 'tramp-remote-path "/run/current-system/sw/bin"))

;; (use-package treemacs
;;   :commands treemacs)

;; (use-package treemacs-projectile
;;   :after treemacs projectile
;;   :ensure t)

  
(provide 'myinit)
;;; myinit ends here
