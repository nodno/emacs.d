;;; package --- Summary
;;; Commentary:

;;; Code:
(require 'package)
(setq package-enable-at-startup nil)
(add-to-list 'package-archives '("melpa" . "https://melpa.org/packages/") t)
;; (add-to-list 'package-archives
;; 	         '("melpa-china" . "https://elpa.emacs-china.org/melpa/"))

(package-initialize)

;; Bootstrap `use-package'
(unless (package-installed-p 'use-package)
  (package-refresh-contents)
  (package-install 'use-package))

(eval-and-compile
  (setq load-path
        (append (delete-dups load-path)
                '("~/.emacs.d/lisp"))))
(load "~/.emacs.d/setting.el")
(load "~/.emacs.d/myinit.el" t)
(load-file "~/.emacs.d/macros.el")

(provide 'init)
;;; init.el ends here

(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(custom-safe-themes
   '("a27c00821ccfd5a78b01e4f35dc056706dd9ede09a8b90c6955ae6a390eb1c1e" "3c83b3676d796422704082049fc38b6966bcad960f896669dfc21a7a37a748fa" default))
 '(gdb-many-windows t)
 '(gdb-show-main t)
 '(lsp-enable-snippet nil)
 '(lsp-prefer-flymake nil t)
 '(org-agenda-files '("~/notes/sicp.org" "~/Dropbox/notes/index.org"))
 '(package-selected-packages
   '(org-pdftools org lispy eyebrowse counsel-osx-app go-dlv dap-mode hl-todo company-irony-c-headers company-irony irony thrift slime-company dired-toggle-sudo sudo-edit imenu-list lsp-vue company-quickhelp smartparens smartparens-config ido-springboard sqlformat company-lsp diredfl dired-x gitlab-ci-mode-flycheck gitlab-ci-mode hs-minor-mode company-c-headers sr-speedbar function-args helm-gtags ob-go c-mode google-c-style smart-mode-line iedit aggressive-indent bm beacon ascii abbrev helm-lsp yasnippet-snippets xref-js2 wsd-mode which-key web-mode use-package try treemacs smart-cursor-color showtip selected regex-tool pyvenv protobuf-mode pos-tip php-mode phi-search-mc ox-twbs org-pdfview nginx-mode magit macrostep lua-mode lsp-ui link-hint leetcode js2-refactor htmlize helm-projectile helm-ls-git helm-descbinds helm-dash go-playground git-gutter flycheck-color-mode-line expand-region exec-path-from-shell diminish deft dash-at-point command-log-mode color-theme-sanityinc-tomorrow blacken avy-zap ace-mc))
 '(safe-local-variable-values
   '((eval setq flycheck-clang-include-path
           (list
            (expand-file-name "/Users/bytedance/workspace/git/glibc/include/")
            (expand-file-name "/Users/bytedance/workspace/git/glibc/")))
     (eval setq flycheck-clang-include-path
           (list
            (expand-file-name "/Users/bytedance/workspace/git/redis/deps/lua/src/"))))))


(put 'upcase-region 'disabled nil)
(put 'set-goal-column 'disabled nil)

(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(erc-input-face ((t (:foreground "antique white"))))
 '(helm-selection ((t (:background "ForestGreen" :foreground "black"))))
 '(org-agenda-clocking ((t (:inherit secondary-selection :foreground "black"))))
 '(org-agenda-done ((t (:foreground "dim gray" :strike-through nil))))
 '(org-clock-overlay ((t (:background "SkyBlue4" :foreground "black"))))
 '(org-done ((t (:foreground "PaleGreen" :weight normal :strike-through t))))
 '(org-headline-done ((((class color) (min-colors 16) (background dark)) (:foreground "LightSalmon" :strike-through t))))
 '(outline-1 ((t (:inherit font-lock-function-name-face :foreground "cornflower blue")))))
