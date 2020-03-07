;;; package --- Summary
;;; Commentary:

;;; Code:
(require 'package)
(setq package-enable-at-startup nil)
(add-to-list 'package-archives
	     '("melpa-china" . "https://elpa.emacs-china.org/melpa/"))

(package-initialize)

;; Bootstrap `use-package'
(unless (package-installed-p 'use-package)
  (package-refresh-contents)
  (package-install 'use-package))

(eval-and-compile
  (setq load-path
        (append (delete-dups load-path)
                '("~/.emacs.d/lisp"))))

(load "~/.emacs.d/myinit.el" t)
;;(org-babel-load-file (expand-file-name "~/.emacs.d/myinit.org"))

;; (custom-set-variables
;;  ;; custom-set-variables was added by Custom.
;;  ;; If you edit it by hand, you could mess it up, so be careful.
;;  ;; Your init file should contain only one such instance.
;;  ;; If there is more than one, they won't work right.
;;  '(ansi-color-faces-vector
;;    [default bold shadow italic underline bold bold-italic bold])
;;  '(auth-source-save-behavior nil)
;;  '(beacon-color "#d54e53")
;;  '(custom-safe-themes
;;    (quote
;;     ("3c83b3676d796422704082049fc38b6966bcad960f896669dfc21a7a37a748fa" "1b8d67b43ff1723960eb5e0cba512a2c7a2ad544ddb2533a90101fd1852b426e" default)))
;;  '(flycheck-color-mode-line-face-to-color (quote mode-line-buffer-id))
;;  '(frame-background-mode (quote dark))
;;  '(helm-dash-browser-func (quote eww))
;;  '(helm-dash-docsets-path
;;    "/Users/zhaoweipu/Library/Application Support/Dash/DocSets/")
;;  '(initial-scratch-message nil)
;;  '(package-selected-packages
;;    (quote
;;     (web-mode macrostep xref-js2 js2-refactor js2-mode sudo-edit protobuf-mode smart-cursor-color go-playground company-lsp lsp-ui lsp-mode flycheck-golangci-lint go-mode leetcode exec-path-from-shell: multi-term mule link-hint treemacs-projectile dash-at-point helm-dash treemacs tramp-sh regex-tool command-log-mode ace-mc selected phi-search-mc multiple-cursors phi-search avy-zap helm-ag company-anaconda company helm-descbinds helm-projectile projectile helm-ls-git helm winner-mode wsd-mode ox-jira ox-md ox-twbs pos-tip popup showtip solarized-theme php-mode mu4e-alert org-pdfview nginx-mode smart-mode-line lua-mode lua git-gutter deft diminish moe-theme zenburn-theme yasnippet-snippets which-key use-package undo-tree try poet-theme org-bullets magit leuven-theme iedit hungry-delete htmlize flycheck-color-mode-line expand-region doom-modeline color-theme-sanityinc-tomorrow color-theme-modern beacon ace-window)))
;;  '(tramp-shell-prompt-pattern
;;    "\\(?:^\\|\\)[^]#$%>
;; ]*#?[]#$%>].* *\\(\\[[0-9;]*[a-zA-Z] *\\)*"))

;; (custom-set-faces
;;  ;; custom-set-faces was added by Custom.
;;  ;; If you edit it by hand, you could mess it up, so be careful.
;;  ;; Your init file should contain only one such instance.
;;  ;; If there is more than one, they won't work right.
;;  '(aw-leading-char-face ((t (:inherit ace-jump-face-foreground :height 3.0))))
;;  '(helm-selection ((t (:background "systemPurpleColor" :foreground "white")))))
;; (put 'downcase-region 'disabled nil)
(provide 'init)
;;; init.el ends here
(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(package-selected-packages
   (quote
    (helm-lsp yasnippet-snippets xref-js2 wsd-mode which-key web-mode use-package try treemacs sudo-edit smart-cursor-color showtip selected regex-tool pyvenv protobuf-mode pos-tip php-mode phi-search-mc ox-twbs org-pdfview org-bullets nginx-mode magit macrostep lua-mode lsp-ui link-hint leetcode js2-refactor htmlize helm-projectile helm-ls-git helm-descbinds helm-dash helm-ag go-playground git-gutter flycheck-color-mode-line expand-region exec-path-from-shell diminish deft dash-at-point company-lsp company-anaconda command-log-mode color-theme-sanityinc-tomorrow blacken avy-zap ace-mc))))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(helm-selection ((t (:background "systemPurpleColor" :foreground "white")))))
(put 'upcase-region 'disabled nil)
(put 'set-goal-column 'disabled nil)
