;;; dot-org.el --- Config file for org-mode
;;; Commentary:
;;;

;;; Code:

(use-package org-bullets
;;  :defer t
  :bind (("C-c l" . org-store-link)
	 ("C-c a" . org-agenda)
	 ("C-c c" . org-iswitchb))
  :hook (org-mode . org-bullets-mode))

(require 'org)

(use-package ob-go
  :defer t)

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

;; (require 'cl)				;for delete*
;; (setq org-emphasis-alist
;;       (cons '("+" (:strike-through t :foreground "gray"))
;; 	    (delete* "+" org-emphasis-alist :key 'car :test 'equal)))
;; (setq org-emphasis-alist
;;       (cons '("*" (bold :foreground "red"))
;; 	    (delete* "*" org-emphasis-alist :key 'car :test 'equal)))
;; 使得中英文表格对其, 需要先安装https://www.google.co.kr/get/noto/
;; (set-face-attribute 'org-table nil :family "Noto Sans Mono CJk SC")

;; for python
(org-babel-do-load-languages
 'org-babel-load-languages
 '((python . t)
   (perl . t)
   (emacs-lisp . t)
   (go . t)
   (shell . t)))
;; Refiling according to the document’s hierarchy.
(setq org-refile-use-outline-path t)
(setq org-outline-path-complete-in-steps nil)
(setq org-src-tab-acts-natively t)

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

(use-package ox-reveal
  :defer 5
  :load-path "~/workspace/git/org-reveal")
;;      :load-path "lisp")
;;      :hook org-mode)

;;(setq org-reveal-root "http://cdn.jsdelivr.net/reveal.js/3.0.0/")
(setq org-reveal-root "https://cdn.jsdelivr.net/npm/reveal.js@3.8.0/")
;;(setq org-reveal-root "file:/Users/zhaoweipu/workspace/git/reveal.js/")
(setq org-reveal-mathjax t)


(provide 'dot-org)
;;; dot-org.el ends here
