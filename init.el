;; my stuff
(require 'package)
(setq package-enable-at-startup nil)
(add-to-list 'package-archives
	     '("melpa-china" . "https://elpa.emacs-china.org/melpa/"))

(package-initialize)

;; Bootstrap `use-package'
(unless (package-installed-p 'use-package)
  (package-refresh-contents)
  (package-install 'use-package))

(org-babel-load-file (expand-file-name "~/.emacs.d/myinit.org"))

(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(ansi-color-faces-vector
   [default bold shadow italic underline bold bold-italic bold])
 '(auth-source-save-behavior nil)
 '(beacon-color "#d54e53")
 '(custom-safe-themes
   (quote
    ("3c83b3676d796422704082049fc38b6966bcad960f896669dfc21a7a37a748fa" "1b8d67b43ff1723960eb5e0cba512a2c7a2ad544ddb2533a90101fd1852b426e" default)))
 '(flycheck-color-mode-line-face-to-color (quote mode-line-buffer-id))
 '(frame-background-mode (quote dark))
 '(package-selected-packages
   (quote
    (winner-mode wsd-mode ox-jira ox-md ox-twbs pos-tip popup showtip solarized-theme php-mode mu4e-alert org-pdfview nginx-mode smart-mode-line lua-mode lua git-gutter deft diminish moe-theme zenburn-theme yasnippet-snippets which-key use-package undo-tree try poet-theme org-bullets magit leuven-theme iedit hungry-delete htmlize flycheck-color-mode-line expand-region elpy doom-modeline counsel company-jedi color-theme-sanityinc-tomorrow color-theme-modern beacon ace-window)))
 '(tramp-shell-prompt-pattern
   "\\(?:^\\|\\)[^]#$%>
]*#?[]#$%>].* *\\(\\[[0-9;]*[a-zA-Z] *\\)*"))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(aw-leading-char-face ((t (:inherit ace-jump-face-foreground :height 3.0)))))
