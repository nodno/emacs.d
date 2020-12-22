;; myinit.el --- Config file for my emacs
;;; Commentary:
;;; My config file, all the packages I'm using, except use-package.

;;; Code:

;;; Functions

(defun nodno/insert-date()
  "insert current date time.
Insert date in this format: yyyy-mm-dd HH:MM:SS.
for example: 2020-10-08 12:10:00."
  (interactive)
  (when (use-region-p) (delete-region (region-beginning) (region-end)))
  (insert
   (format-time-string "%Y-%m-%d %H:%M:%S")))

(global-set-key (kbd "C-c t") 'nodno/insert-date)

(global-set-key (kbd "C-H-f") 'toggle-frame-fullscreen)

;;; Libraries
(use-package diminish)
(use-package deferred      :defer t)
(use-package pkg-info      :defer t)
(use-package popup         :defer t)
(use-package popup-pos-tip :defer t)
(use-package popwin        :defer t)
(use-package pos-tip       :defer t)
(use-package web           :defer t)
(use-package web-server    :defer t)
(use-package websocket     :defer t)
(use-package with-editor   :defer t)
;;; Packages
(use-package abbrev
  :ensure nil
  :hook
  ((js2-mode web-mode) . abbrev-mode))

(use-package ace-jump-mode
  :defer t)

(use-package ace-mc
  :bind (("<C-m> h"   . ace-mc-add-multiple-cursors)
         ("<C-m> M-h" . ace-mc-add-single-cursor)))

(use-package ace-window
  :init (setq aw-swap-invert t)
  :bind (("C-x o" . ace-window)
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

(use-package auto-rename-tag
  :disabled t
  :hook (web-mode . auto-rename-tag-mode))

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
  ;;  :commands beacon-mode
  :config
  (beacon-mode t))

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
  :after posframe
  :preface
  (defvar dw/command-window-frame nil)
  (defun dw/toggle-command-window ()
    (interactive)
    (if dw/command-window-frame
        (progn
          (posframe-delete-frame clm/command-log-buffer)
          (setq dw/command-window-frame nil))
      (progn
        (global-command-log-mode t)
        (with-current-buffer
            (setq clm/command-log-buffer
                  (get-buffer-create " *command-log*"))
          (text-scale-set -1))
        (setq dw/command-window-frame
              (posframe-show
               clm/command-log-buffer
               :position `(,(- (x-display-pixel-width) 650) . 50)
               :width 35
               :height 5
               :min-width 35
               :min-height 5
               :internal-border-width 2
               :internal-border-color "#c792ea"
               :override-parameters '((parent-frame . nil)))))))
  :config
  (setq clm/log-command-exceptions* nil))


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

(use-package company-c-headers
  :config
  (add-to-list 'company-backends 'company-c-headers))

(use-package company-quickhelp
  :after company
  :hook (company-mode . company-quickhelp-mode))
;; :bind (:map company-active-map
;;             ("C-c ?" . company-quickhelp-manual-begin)))

(use-package company-irony
  :after (company irony-mode)
  :config
  (add-to-list 'company-backends 'company-irony))

(use-package company-irony-c-headers
  :after (company-irony)
  :config
  (add-to-list 'company-backends 'company-irony-c-headers))

(use-package counsel
  :disabled t
  :after ivy
  :bind
  ("M-y" . counsel-yank-pop))

(use-package dap-mode
  :config
  (require 'dap-go)
  (setq dap-print-io t) 
  (dap-go-setup)
  (dap-mode 1)
  (dap-ui-mode 1)
  ;; enables mouse hover support
  (dap-tooltip-mode 1)
  ;; use tooltips for mouse hover
  ;; if it is not enabled `dap-mode' will use the minibuffer.
  (tooltip-mode 1)
  ;; displays floating panel with debug buttons
  ;; requies emacs 26+
  (dap-ui-controls-mode 1)

  (require 'dap-hydra)
  (add-hook 'dap-stopped-hook
            (lambda (arg) (call-interactively #'dap-hydra))))


(use-package dash-at-point
  :bind ("C-c D" . dash-at-point)
  :config
  (add-to-list 'dash-at-point-mode-alist
               '(lisp-mode . "Common Lisp")
               
	           '(python-mode . "Python3")))

(use-package deft
  :bind ("C-c d" . deft)
  :commands (deft)
  :config
  (setq deft-directory "~/notes"
        deft-extensions '("org")
        deft-default-extension "org"
        deft-use-filename-as-title t
        deft-use-filter-string-for-filename t))

(use-package go-dlv
  :ensure t)

(use-package diredfl
  :hook (dired-mode . diredfl-mode))

(use-package dot-org
  :load-path "lisp")


(use-package eldoc
  :diminish)

(use-package eglot
  :disabled
  :hook
  (go-mode . eglot-ensure)
  :config
  (defvar-local flycheck-eglot-current-errors nil)

  (defun flycheck-eglot-report-fn (diags &rest _)
    (setq flycheck-eglot-current-errors
          (mapcar (lambda (diag)
                    (save-excursion
                      (goto-char (flymake--diag-beg diag))
                      (flycheck-error-new-at (line-number-at-pos)
                                             (1+ (- (point) (line-beginning-position)))
                                             (pcase (flymake--diag-type diag)
                                               ('eglot-error 'error)
                                               ('eglot-warning 'warning)
                                               ('eglot-note 'info)
                                               (_ (error "Unknown diag type, %S" diag)))
                                             (flymake--diag-text diag)
                                             :checker 'eglot)))
                  diags))
    (flycheck-buffer))

  (defun flycheck-eglot--start (checker callback)
    (funcall callback 'finished flycheck-eglot-current-errors))

  (defun flycheck-eglot--available-p ()
    (bound-and-true-p eglot--managed-mode))

  (flycheck-define-generic-checker 'eglot
    "Report `eglot' diagnostics using `flycheck'."
    :start #'flycheck-eglot--start
    :predicate #'flycheck-eglot--available-p
    :modes '(prog-mode text-mode))

  (push 'eglot flycheck-checkers)

  (defun sanityinc/eglot-prefer-flycheck ()
    (when eglot--managed-mode
      (flycheck-add-mode 'eglot major-mode)
      (flycheck-select-checker 'eglot)
      (flycheck-mode)
      (flymake-mode -1)
      (setq eglot--current-flymake-report-fn 'flycheck-eglot-report-fn)))

  (add-hook 'eglot--managed-mode-hook 'sanityinc/eglot-prefer-flycheck)  
  )

;; (setq-default eglot-workspace-configuration
;;               '((:gopls .
;;                         ((staticcheck . t)
;;                          (matcher . "CaseSensitive")))))


;; (defun eglot-format-buffer-on-save ()
;;   (add-hook 'before-save-hook #'eglot-format-buffer -10 t))

;; (add-hook 'go-mode-hook #'eglot-format-buffer-on-save)


(use-package eyebrowse
  :bind-keymap ("C-\\" . eyebrowse-mode-map)
  :bind (:map eyebrowse-mode-map
              ("C-\\ C-\\" . eyebrowse-last-window-config)
              ("1" . eyebrowse-switch-to-window-config-1)
              ("2" . eyebrowse-switch-to-window-config-2)
              ("3" . eyebrowse-switch-to-window-config-3)
              ("4" . eyebrowse-switch-to-window-config-4))
  :config
  (eyebrowse-mode t))


(use-package go
  :disabled
  :load-path "lisp/el-go"
  )

(use-package elpy
  :disabled t
  :defer t
  :init
  (elpy-enable)
  :hook
  (python-mode . elpy-mode)
  :config
  (setq eldoc-idle-delay 1)
  (setq python-shell-interpreter "ipython"
        python-shell-interpreter-args "-i --simple-prompt")  
  (when (require 'flycheck nil t)
    (setq elpy-modules (delq 'elpy-module-flymake elpy-modules)))

  ;; force it to use black, as there this function in elpy.el seems
  ;; can't find black
  (defun elpy-format-code ()
    "Format code using the available formatter."
    (interactive)
    (elpy-black-fix-code)))

(use-package exec-path-from-shell
  :config (dolist (var '("SSH_AUTH_SOCK" "SSH_AGENT_PID" "GPG_AGENT_INFO" "LANG" "LC_CTYPE"))
            (add-to-list 'exec-path-from-shell-variables var)
            (setq exec-path-from-shell-check-startup-files nil))
  (exec-path-from-shell-initialize))

(use-package expand-region
  :bind ("C-=" . er/expand-region))

(use-package f
  :defer t)

(use-package flycheck
  :hook
  (prog-mode . flycheck-mode))

(use-package flycheck-color-mode-line
  :hook (flycheck-mode . flycheck-color-mode-line-mode))

(use-package function-args
  :config(fa-config-default))

(use-package ggtags
  :disabled
  :hook
  (c-mode-common . (lambda ()
                     (when (derived-mode-p 'c-mode 'c++-mode 'java-mode 'asm-mode)
                       (ggtags-mode 1))))
  :bind (:map ggtags-mode-map
              ("C-c g s" . ggtags-find-other-symbol)
              ("C-c g h" . ggtags-view-tag-history)
              ("C-c g r" . ggtags-find-reference)
              ("C-c g f" . ggtags-find-file)
              ("C-c g c" . ggtags-create-tags)
              ("C-c g u" . ggtags-update-tags)
              ("M-," . pop-tag-mark)))


(use-package git-gutter
  :disabled
  :defer 1
  :diminish git-gutter-mode
  :init
  (global-git-gutter-mode +1))

(use-package gitlab-ci-mode
  :defer 2)

(use-package gitlab-ci-mode-flycheck
  :after flycheck gitlab-ci-mode
  :init
  (gitlab-ci-mode-flycheck-enable))

(use-package go-mode
  :mode ("\\.go\\'" . go-mode)
  :config
  (define-key go-mode-map  (kbd "C-c C-o") 'zwp/go-occour-definitions)
  (add-hook 'go-mode-hook (lambda () (setq tab-width 4)))
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
  )

(use-package go-playground
  :bind ("C-c g" . go-playground-exec))



(use-package google-c-style
  :config
  (add-hook 'c-mode-common-hook 'google-set-c-style)
  (add-hook 'c-mode-common-hook 'google-make-newline-indent))

(defun my-c-mode-hook ()
  (setq c-basic-offset 4
        ;;        c-indent-level 4
        c-default-style "google"))
(add-hook'c-mode-common-hook 'my-c-mode-hook)

(use-package helm
  ;; based on http://tuhdo.github.io/helm-intro.html
  ;;  :diminish (helm-mode)
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
  (global-set-key (kbd "C-c h g") 'helm-google-suggest)  
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
        helm-M-x-fuzzy-match t
        helm-dwim-target 'next-window
        helm-ff-auto-update-initial-value 1)
  (helm-autoresize-mode 1)

  (helm-mode 1))

(use-package helm-dash
  ;;  fixed dash-doc.el temporary-file-directory to /tmp/ for Catalina
  :commands helm-dash
  :config
  (setq helm-dash-enable-debugging t)
  (setq helm-dash-browser-func (quote eww))
  (setq helm-dash-docsets-path "/Users/bytedance/Library/Application Support/Dash/DocSets/")
  (add-to-list 'helm-dash-common-docsets "Go")  
  ;; (add-to-list 'helm-dash-common-docsets "Django")
  ;; (add-to-list 'helm-dash-common-docsets "Python 2")
  (add-to-list 'helm-dash-common-docsets "Python 3")
  (add-to-list 'helm-dash-common-docsets "JavaScript")  
  (add-to-list 'helm-dash-common-docsets "Redis"))

(use-package helm-descbinds
  :defer 7
  :config
  (helm-descbinds-mode))

(use-package helm-gtags
  :config (setq
           helm-gtags-ignore-case t
           helm-gtags-auto-update t
           helm-gtags-use-input-at-cursor t
           helm-gtags-pulse-at-cursor t
           helm-gtags-prefix-key "\C-cg"
           helm-gtags-suggested-key-mapping t)
  :hook ((dired-mode . helm-gtags-mode)
         (eshell-mode . helm-gtags-mode)
         (c-mode . helm-gtags-mode)
         (python-mode . helm-gtags-mode)         
         (c++-mode . helm-gtags-mode)
         (java-mode . helm-gtags-mode)
         (asm-mode . helm-gtags-mode))
  :bind (:map helm-gtags-mode-map
              ("C-c g a" . helm-gtags-tags-in-this-function)
              ("C-j" . helm-gtags-select)
              ("M-." . helm-gtags-dwim)
              ("M-," . helm-gtags-pop-stack)
              ("C-c <" . helm-gtags-previous-history)
              ("C-c >" . helm-gtags-next-history)
              ("\C-x4." . helm-gtags-find-tag-other-window)))


(use-package helm-ls-git
  :after
  (helm-mode)
  :config
  (global-set-key (kbd "C-x C-d") 'helm-browse-project))


(use-package hideshow
  :diminish hs-minor-mode
  :commands hs-minor-mode
  :hook (prog-mode . hs-minor-mode)
  :bind (:map prog-mode-map
              ("M-[" . hs-toggle-hiding)))

(use-package hl-todo
  :config
  (setq hl-todo-keyword-faces
        '(("TODO"   . "#FF0000")
          ("FIXME"  . "#FF0000")
          ("DEBUG"  . "#A020F0")
          ("GOTCHA" . "#FF4500")
          ("STUB"   . "#1E90FF")))
  (global-hl-todo-mode)
  )


(use-package htmlize)
(use-package hungry-delete
  :disabled t
  :config
  (global-hungry-delete-mode))

(use-package iedit
  :defer 5)

(use-package imenu-list
  :commands imenu-list-minor-mode)

(use-package indium
  :disabled t
  )

;; == irony-mode ==
(use-package irony
  :ensure t
  :defer t
  :init
  (add-hook 'c++-mode-hook 'irony-mode)
  (add-hook 'c-mode-hook 'irony-mode)
  (add-hook 'objc-mode-hook 'irony-mode)
  :config
  ;; replace the `completion-at-point' and `complete-symbol' bindings in
  ;; irony-mode's buffers by irony-mode's function
  (defun my-irony-mode-hook ()
    (define-key irony-mode-map [remap completion-at-point]
      'irony-completion-at-point-async)
    (define-key irony-mode-map [remap complete-symbol]
      'irony-completion-at-point-async))
  (add-hook 'irony-mode-hook 'my-irony-mode-hook)
  (add-hook 'irony-mode-hook 'irony-cdb-autosetup-compile-options))

(use-package ivy
  :disabled t
  :diminish (ivy-mode)
  :config
  (setq ivy-count-format "%d/%d "))

(use-package js2-mode
  :mode ("\\.js\\'" . js2-mode)
  :config
  (setq js-indent-level 2)
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

(use-package json-mode)

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
  ;;  (lsp-auto-guess-root t)
  (lsp-enable-snippet nil)
  (lsp-prefer-flymake nil) ; Use flycheck instead of flymake
  :hook
  ((js-mode css-mode scss-mode typescript-mode web-mode go-mode) . lsp-deferred)
  ;; ((js-mode css-mode scss-mode typescript-mode web-mode) . lsp-deferred)  
  (lsp-mode . (lambda ()
                (let ((lsp-keymap-prefix "H-l"))
                  (lsp-enable-which-key-integration))))

  :config
  ;; (setq lsp-gopls-staticcheck t)
  ;; (setq lsp-eldoc-render-all t)
  ;; (setq lsp-gopls-complete-unimported t)
  (define-key lsp-mode-map (kbd "H-l") lsp-command-map)
  (use-package helm-lsp :commands helm-lsp-workspace-symbol)

  ;; Optional - provides fancier overlays.
  (use-package lsp-ui
    :init
    (setq lsp-ui-doc-position 'top)
    :commands lsp-ui-mode))

;; Set up before-save hooks to format buffer and add/delete imports.
;; Make sure you don't have other gofmt/goimports hooks enabled.
(defun lsp-go-install-save-hooks ()
  (add-hook 'before-save-hook #'lsp-format-buffer t t)
  (add-hook 'before-save-hook #'lsp-organize-imports t t))
(add-hook 'go-mode-hook #'lsp-go-install-save-hooks)


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

(use-package tern :ensure t :defer 30
  :if (locate-file "tern" exec-path)
  :hook (js2-mode . tern-mode))

(use-package thrift
  :ensure t
  :defer t)

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


(use-package posframe)

(use-package prettier-js
  :hook
  (web-mode . prettier-js-mode))

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
  (setq-default indent-tabs-mode nil)
  (setq python-indent-guess-indent-offset t)
  (setq python-indent-guess-indent-offset-verbose nil))

(use-package pyvenv
  :hook (python-mode . pyvenv-mode)
  :config
  (setenv "WORKON_HOME" "/Users/bytedance/opt/anaconda3/envs")
  (pyvenv-workon "py2"))

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

(use-package slime
  :commands slime
  :init
  (setq inferior-lisp-program "clisp"
        slime-contribs '(slime-fancy)))



(use-package slime-company
  :after (slime company)
  :config (setq slime-company-completion 'fuzzy
                slime-company-after-completion 'slime-company-just-one-space))

(slime-setup '(slime-fancy slime-company))

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

(use-package smartparens
  :config
  (setq sp-show-pair-from-inside nil)
  (require 'smartparens-config)
  :diminish smartparens-mode
  :hook
  (program-mode . smartparens-mode))


(use-package sr-speedbar)

;; brew install pgformatter
(use-package sqlformat
  :config
  (setq sqlformat-command 'pgformatter)
  (setq sqlformat-args '("-s2" "-g")))

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

(use-package tide
  :ensure t
  :after (typescript-mode company flycheck)
  :hook ((typescript-mode . tide-setup)
         (typescript-mode . tide-hl-identifier-mode)
         (before-save . tide-format-before-save)))


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
  :unless noninteractive
  :defer 5
  :bind (("M-N" . winner-redo)
         ("M-P" . winner-undo))
  :config
  (winner-mode 1))

(use-package web-mode
  :defer 5
  :bind
  ("C-c C-v" . browse-url-of-buffer)
  :config
  (add-to-list 'auto-mode-alist '("\\.html?\\'" . web-mode))
  (add-to-list 'auto-mode-alist '("\\.ts?\\'" . web-mode))  
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
  (setq web-mode-markup-indent-offset 2)
  (setq web-mode-code-indent-offset 2)
  (setq web-mode-enable-auto-closing t)
  (setq indent-tabs-mode nil)
  (setq web-mode-enable-auto-quoting t)) ;; this fixes the quote problem I mentioned

;; (defun my-web-mode-hook ()
;;   "Hooks for Web mode."
;;   (setq web-mode-markup-indent-offset 2)
;;   )
;; (add-hook 'web-mode-hook  'my-web-mode-hook)

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
