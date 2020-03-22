;; myinit.el --- Config file for my emacs
;;; Commentary:
;;; My config file, all the packages I'm using, except use-package.

;;; Code:
;;; Libraries
(use-package diminish)
;;; Packages
(use-package ace-jump-mode
  :defer t)

(use-package ace-mc
  :bind (("<C-m> h"   . ace-mc-add-multiple-cursors)
         ("<C-m> M-h" . ace-mc-add-single-cursor)))

(use-package ace-window
  :init (setq aw-swap-invert t)
  :bind* (("C-<return>" . ace-window)
          ("M-O" . ace-swap-window)))

(use-package aggressive-indent
  :diminish
  :hook (emacs-lisp-mode . aggressive-indent-mode))

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
  (setq anaconda-mode-localhost-address "localhost")
  ;; as C-c C-o is so handy in elpy, I'll keep it with anaconda-mode
  (defun elpy-occur-definitions ()
    "Display an occur buffer of all definitions in the current buffer. Also, switch to that buffer."
    (interactive)
    (let ((list-matching-lines-face nil))
      (occur "^\s*\\(\\(async\s\\|\\)def\\|class\\)\s"))
    (let ((window (get-buffer-window "*Occur*")))
      (if window
          (select-window window)
        (switch-to-buffer "*Occur*")))))



(use-package avy
  ;; Avy - navigate by searching for a letter on the screen and jumping to it
  :bind* ("C-." . avy-goto-char-timer)
  :config
  (avy-setup-default))

(use-package avy-zap
  :bind
  ("M-z" . avy-zap-to-char-dwim)
  ("M-Z" . avy-zap-up-to-char-dwim))

(use-package beacon
  :diminish
  :commands beacon-mode)

(use-package blacken
  :ensure t
  :bind ("C-c b" . blacken-buffer))

(use-package cc-mode
  :mode (("\\.h\\(h?\\|xx\\|pp\\)\\'" . c++-mode)
         ("\\.m\\'" . c-mode)
         ("\\.mm\\'" . c++-mode)))
  ;; :bind (:map c-mode-map
  ;;             ("<tab>" . company-complete))
  ;; :bind (:map c++-mode-map
  ;;             ("<tab>" . company-complete)))

(use-package change-inner
  :bind (("M-i"     . change-inner)
         ("M-o M-o" . change-outer)))


(use-package color-theme-modern
  :disabled t)

(use-package color-theme-sanityinc-tomorrow
  :config
  (load-theme 'sanityinc-tomorrow-bright t))

(use-package command-log-mode
  :bind (("C-c e M" . command-log-mode)
         ("C-c e L" . clm/open-command-log-buffer))
  :config (setq clm/log-command-exceptions* nil))


(use-package company
  :diminish (company-mode)
  :hook (prog-mode . company-mode)
  :config
  (setq company-idle-delay 0)
  (setq company-minimum-prefix-length 1)
  (setq company-backends (delete 'company-semantic company-backends)))


(use-package company-anaconda
  :after (company anaconda-mode)
  :config
  (push 'company-anaconda company-backends))

(use-package counsel
  :disabled t
  :after ivy
  :bind
  ("M-y" . counsel-yank-pop))

(use-package dash-at-point
  :bind ("C-c D" . dash-at-point)
  :config
  (add-to-list 'dash-at-point-mode-alist
	       '(python-mode . "python")))

(use-package deft
  :bind ("C-c d" . deft)
  :commands (deft)
  :config
  (setq deft-directory "~/Dropbox/notes"
	deft-extensions '("org")
	deft-default-extension "org"
	deft-use-filename-as-title t
	deft-use-filter-string-for-filename t))


(use-package dot-org
  :load-path "lisp")



(use-package eldoc
  :diminish)

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

(use-package exec-path-from-shell
  :config (dolist (var '("SSH_AUTH_SOCK" "SSH_AGENT_PID" "GPG_AGENT_INFO" "LANG" "LC_CTYPE"))
            (add-to-list 'exec-path-from-shell-variables var))
  (exec-path-from-shell-initialize))

(use-package expand-region
  :bind ("C-=" . er/expand-region))

(use-package flycheck
  :hook
  (prog-mode . flycheck-mode))

(use-package flycheck-color-mode-line
  :hook (flycheck-mode . flycheck-color-mode-line-mode))

(use-package git-gutter
  :disabled t
  :defer 1
  :diminish git-gutter-mode
  :init
  (global-git-gutter-mode +1))

(use-package go-mode
  :mode ("\\.go\\'" . go-mode)
  :config
  ;; TODO: check where this C-c C-o doesn't work
  (define-key go-mode-map  (kbd "C-c C-o") 'zwp/go-occour-definitions)
  (add-hook 'go-mode-hook (lambda () (setq tab-width 4)))
  (add-hook 'go-mode-hook #'lsp-go-install-save-hooks)
  :preface
  (defun zwp/go-occour-definitions()
    "Display an occur buffer of all definitions in the current buffer. Also, switch to that buffer."
    (interactive)
    (let ((list-matching-lines-face nil))
      (occur "^\s*\\(type\\|func\\|var\\|const\\)\s"))
    (let ((window (get-buffer-window "*Occur*")))
      (if window
          (select-window window)
        (switch-to-buffer "*Occur*"))))
  ;; Set up before-save hooks to format buffer and add/delete imports.
  ;; Make sure you don't have other gofmt/goimports hooks enabled.
  (defun lsp-go-install-save-hooks ()
    (add-hook 'before-save-hook #'lsp-format-buffer t t)
    (add-hook 'before-save-hook #'lsp-organize-imports t t)))

(use-package go-playground
  :bind ("C-c g" . go-playground-exec))

(use-package google-c-style
  :config
  (add-hook 'c-mode-common-hook 'google-set-c-style)
  (add-hook 'c-mode-common-hook 'google-make-newline-indent))


(use-package helm
  ;; based on http://tuhdo.github.io/helm-intro.html
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

  ;; (custom-set-faces
  ;;  '(helm-selection ((t (:background "systemPurpleColor" :foreground "white")))))
  (helm-mode 1))

(use-package helm-dash
  ;;  fixed dash-doc.el temporary-file-directory to /tmp/ for Catalina
  :commands helm-dash
  :config
  (setq helm-dash-enable-debugging t)
  (setq helm-dash-browser-func (quote eww))
  (setq helm-dash-docsets-path "/Users/zhaoweipu/Library/Application Support/Dash/DocSets/")
  (add-to-list 'helm-dash-common-docsets "Go")  
  ;; (add-to-list 'helm-dash-common-docsets "Django")
  ;; (add-to-list 'helm-dash-common-docsets "Python 2")
  ;; (add-to-list 'helm-dash-common-docsets "Python 3")
  ;; (add-to-list 'helm-dash-common-docsets "Python 3")
  (add-to-list 'helm-dash-common-docsets "Redis"))

(use-package helm-descbinds
  :defer 7
  :config
  (helm-descbinds-mode))

(use-package helm-ls-git
  :after
  (helm-mode)
  :config
  (global-set-key (kbd "C-x C-d") 'helm-browse-project))

(use-package htmlize)
(use-package hungry-delete
  :disabled t
  :config
  (global-hungry-delete-mode))

(use-package iedit
  :defer 5)

(use-package ivy
  :disabled t
  :diminish (ivy-mode)
  :config
  (setq ivy-count-format "%d/%d "))

(use-package js2-mode
  :mode ("\\.js\\'" . js2-mode)
  :config
  (setq js-indent-level 4)
  (use-package js2-refactor)
  (use-package xref-js2)
  (add-hook 'js2-mode-hook #'js2-refactor-mode)
  (js2r-add-keybindings-with-prefix "C-c C-r")
  (define-key js2-mode-map (kbd "C-k") #'js2r-kill)
  ;; js-mode (which js2 is based on) binds "M-." which conflicts with xref, so
  ;; unbind it.
  (define-key js-mode-map (kbd "M-.") nil)

  (add-hook 'js2-mode-hook (lambda ()
                             (add-hook 'xref-backend-functions #'xref-js2-xref-backend nil t))))

(use-package leetcode
  :bind
  ("C-x l" . leetcode)
  :config
  (setq leetcode-prefer-language "python3"))

(use-package link-hint
  ;;  :defer 10
  :bind ("C-c C-o" . link-hint-open-link)
  :config
  (add-hook 'eww-mode-hook
            #'(lambda () (bind-key "f" #'link-hint-open-link eww-mode-map))))

(use-package linum
  :hook
  (prog-mode . linum-mode))

(use-package lua-mode
  :mode "\\.lua\\'"
  :interpreter "lua"
  :config
  (setq lua-indent-level 4))

(use-package lsp-mode
  :commands (lsp lsp-deferred)
  :custom
  ;; (lsp-auto-guess-root t)
  (lsp-enable-snippet nil)
  (lsp-prefer-flymake nil) ; Use flycheck instead of flymake
  :hook (go-mode . lsp-deferred)
  :config
  (use-package company-lsp
    :commands company-lsp)

  (use-package helm-lsp :commands helm-lsp-workspace-symbol)
  ;; :hook ((go-mode . lsp))

  ;; Optional - provides fancier overlays.
  (use-package lsp-ui
    :init
    (setq lsp-ui-doc-position 'top)
    :commands lsp-ui-mode))

(use-package macrostep
  :bind ("C-c e m" . macrostep-expand))

(use-package magit
  :bind
  ("C-x g" . magit-status)
  ("C-x M-g" . magit-dispatch)
  :config
  (setq magit-repository-directories
        '((user-emacs-directory . 0)
          ("~/workspace/github.com" . 2)
          ("~/workspace/git" . 2)
          ("~/go/src/gitlab.p1staff.com" . 2))))

(use-package mule
  :disabled t
  :no-require t
  :config
  (prefer-coding-system 'utf-8)
  (set-terminal-coding-system 'utf-8)
  (setq x-select-request-type '(UTF8_STRING COMPOUND_TEXT TEXT STRING)))

(use-package multiple-cursors
  :after (phi-search selected)
  ;;  :defer 5

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

(use-package multi-term
  :disabled t
  :bind (("C-c t" . multi-term-next)
         ("C-c T" . multi-term))
  :init
  (defun screen ()
    (interactive)
    (let (term-buffer)
      ;; Set buffer.
      (setq term-buffer
            (let ((multi-term-program (executable-find "screen"))
                  (multi-term-program-switches "-DR"))
              (multi-term-get-buffer)))
      (set-buffer term-buffer)
      (multi-term-internal)
      (switch-to-buffer term-buffer)))

  :config
  (require 'term)

  (defalias 'my-term-send-raw-at-prompt 'term-send-raw)

  (defun my-term-end-of-buffer ()
    (interactive)
    (call-interactively #'end-of-buffer)
    (if (and (eobp) (bolp))
        (delete-char -1)))

  (defadvice term-process-pager (after term-process-rebind-keys activate)
    (define-key term-pager-break-map  "\177" 'term-pager-back-page)))


(use-package pdf-tools
  ;; (setenv "PKG_CONFIG_PATH" "/usr/local/lib/pkgconfig:/usr/local/Cellar/libffi/3.2.1/lib/pkgconfig")  
  :defer 6
  :magic ("%PDF" . pdf-view-mode)
  :config
  (pdf-tools-install :no-query))


(use-package personal
  :load-path "lisp"
  :bind (("M-M" . my-open-Messages)
         ("M-T" . my-open-Things3)
         ("M-W" . my-open-WeChat)
         ("M-S" . my-open-Safari)
         ("M-F" . my-open-Finder)))


(use-package phi-search
  :defer 5)

(use-package phi-search-mc
  :after (phi-search multiple-cursors)
  :config
  (phi-search-mc/setup-keys)
  (add-hook 'isearch-mode-mode #'phi-search-from-isearch-mc/setup-keys))

(use-package php-mode
  :mode ("\\.php\\'" . php-mode)
  :interpreter ("php" . python-mode))

(use-package popup
  :ensure t)
(use-package pos-tip
  :ensure t)

(use-package protobuf-mode
  :ensure t
  :mode ("\\.proto\\'" . protobuf-mode)
  :config
  (defconst my-protobuf-style
    '((c-basic-offset . 4)
      (indent-tabs-mode . nil)))
  (add-hook 'protobuf-mode-hook
            (lambda () (c-add-style "my-style" my-protobuf-style t))))

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


(use-package python
  ;; The package is "python" but the mode is "python-mode":
  :mode ("\\.py\\'" . python-mode)
  :interpreter ("python" . python-mode)
  :config
  (setq python-indent-guess-indent-offset t)
  (setq python-indent-guess-indent-offset-verbose nil))

(use-package pyvenv
  :hook (python-mode . pyvenv-mode)
  :config
  (setenv "WORKON_HOME" "/Users/zhaoweipu/opt/anaconda3/envs")
  (pyvenv-workon "py3"))

(use-package regex-tool
  :commands regex-tool
  :preface

  (defun regex-tool-perl ()
    "Set perl as backend and run regex-tool."
    (interactive)
    (setq regex-tool-backend 'perl)
    (regex-tool))

  (defun regex-tool-emacs ()
    "Set perl as backend and run regex-tool."
    (interactive)
    (setq regex-tool-backend 'emacs)
    (regex-tool)))


(use-package sdcv
  :load-path "lisp"
  :demand t
  :config
  (global-set-key (kbd "C-x t") 'sdcv-search-pointer))

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

(use-package showtip
  :ensure t)

(use-package smart-cursor-color
  :ensure t
  :defer 3
  :config
  (smart-cursor-color-mode +1))

(use-package smart-mode-line
  :disabled t
  :config
  ;; See https://github.com/Malabarba/smart-mode-line/issues/217
  (setq mode-line-format (delq 'mode-line-position mode-line-format))
  (sml/setup)
  (sml/apply-theme 'light)
  (remove-hook 'display-time-hook 'sml/propertize-time-string))


(use-package smart-newline
  :diminish
  :commands smart-newline-mode)


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

(use-package tramp
  :defer 5
  :config
  ;; jww (2018-02-20): Without this change, tramp ends up sending hundreds of
  ;; shell commands to the remote side to ask what the temporary directory is.
  (put 'temporary-file-directory 'standard-value '("/tmp"))
  (setq tramp-auto-save-directory "~/.cache/emacs/backups"
        ;;tramp-default-user "zhaoweipu"
        tramp-persistency-file-name "~/.emacs.d/data/tramp"))

(use-package tramp-sh
  :load-path "lisp")

(use-package treemacs
  :commands treemacs)

(use-package try
  :defer 10)

(use-package undo-tree
  ;; use C-x u to see the visual undo tree
  ;; use C-x p / n / f b
  ;; q to quit the undo tree visualizer
  :disabled t
  :diminish (undo-tree-mode)
  :init
  (global-undo-tree-mode))

(use-package which-key
  :diminish (which-key-mode)
  :defer 3
  :config
  (which-key-mode))

(use-package winner
  :config
  (winner-mode))

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


(use-package wsd-mode
  :defer 3
  :config
  (setq wsd-style "modern-blue"))

(use-package yasnippet
  :defer 3
  :diminish (yas-minor-mode)
  ;;:after flycheck
  :init
  (yas-global-mode 1)
  :config
  (use-package yasnippet-snippets))


(provide 'myinit)
;;; myinit ends here
