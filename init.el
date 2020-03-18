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

(provide 'init)
;;; init.el ends here

(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(package-selected-packages
   (quote
    (aggressive-indent bm beacon ascii abbrev helm-lsp yasnippet-snippets xref-js2 wsd-mode which-key web-mode use-package try treemacs sudo-edit smart-cursor-color showtip selected regex-tool pyvenv protobuf-mode pos-tip php-mode phi-search-mc ox-twbs org-pdfview org-bullets nginx-mode magit macrostep lua-mode lsp-ui link-hint leetcode js2-refactor htmlize helm-projectile helm-ls-git helm-descbinds helm-dash helm-ag go-playground git-gutter flycheck-color-mode-line expand-region exec-path-from-shell diminish deft dash-at-point company-lsp company-anaconda command-log-mode color-theme-sanityinc-tomorrow blacken avy-zap ace-mc))))


(put 'upcase-region 'disabled nil)
(put 'set-goal-column 'disabled nil)
