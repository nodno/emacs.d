;;; setting --- Summar
;;; Commentary:
;;; all settings
;; cl warning
(setq byte-compile-warnings '(cl-functions))
;; yuse-package-setting:
(setq use-package-always-ensure t)
(setq use-package-verbose t)
;; suppress warning
(setq ad-redefinition-action 'accept)

;; backup copy from SachaChua
(setq backup-directory-alist '(("." . "~/.emacs.d/backups")))
;;emacs-mac
;; https://gist.github.com/railwaycat/3498096
(setq mac-option-modifier 'meta)
(setq mac-command-modifier 'hyper)
(global-set-key [(hyper a)] 'mark-whole-buffer)
(global-set-key [(hyper v)] 'yank)
(global-set-key [(hyper c)] 'kill-ring-save)
(global-set-key [(hyper s)] 'save-buffer)
;;(global-set-key [(hyper l)] 'goto-line)
(global-set-key [(hyper w)]
                (lambda () (interactive) (delete-window)))
(global-set-key [(hyper z)] 'undo)

(setq delete-old-versions -1)
(setq version-control t)
(setq vc-make-backup-files t)
(setq auto-save-file-name-transforms '((".*" "~/.emacs.d/auto-save-list/" t)))

;; interface tweaks
(setq inhibit-startup-message t)
(tool-bar-mode -1)
(scroll-bar-mode -1)
(fset 'yes-or-no-p 'y-or-n-p)
(global-set-key (kbd "<f5>") 'revert-buffer)
;; ns-popup-font-panel was bound to s-t.
(global-unset-key (kbd "s-t"))
(global-hl-line-mode 1)
;; Keymaps

(define-key input-decode-map [?\C-m] [C-m])


;; Setting
(setenv "LANG" "")
(setq-default indent-tabs-mode nil)
(setq-default tab-width 4)
;; (load "~/Dropbox/mu4econfig.el" t)
(if (string= system-type "darwin")
    (progn (delete-file "~/Library/Colors/Emacs.clr")
           ;;delete on macos

           (setq dired-use-ls-dired nil)
           (setq delete-by-moving-to-trash t)
           (setq trash-directory "~/.Trash")))

(define-key global-map (kbd "C-z") nil)

;; (setq enable-recursive-minibuffers t)
(add-to-list 'Info-additional-directory-list "~/.local/share/info")
;; [[http://pragmaticemacs.com/emacs/add-the-system-clipboard-to-the-emacs-kill-ring/][ADD THE SYSTEM CLIPBOARD TO THE EMACS KILL-RING]]
(setq save-interprogram-paste-before-kill t)

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
                                        ;
;; Opacity
(defun sanityinc/adjust-opacity (frame incr)
  "Adjust-opacity copied from SachaChua.works on the current FRAME, INCR by 2/-1."
  (let* ((oldalpha (or (frame-parameter frame 'alpha) 100))
         n         (newalpha (+ incr oldalpha)))
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

(when window-system
  (custom-set-faces
   '(erc-input-face ((t (:foreground "antique white"))))
   '(helm-selection ((t (:background "ForestGreen" :foreground "black"))))
   '(org-agenda-clocking ((t (:inherit secondary-selection :foreground "black"))) t)
   '(org-agenda-done ((t (:foreground "dim gray" :strike-through nil))))
   '(org-done ((t (:foreground "PaleGreen" :weight normal :strike-through t))))
   '(org-clock-overlay ((t (:background "SkyBlue4" :foreground "black"))))
   '(org-headline-done ((((class color) (min-colors 16) (background dark)) (:foreground "LightSalmon" :strike-through t))))
   '(outline-1 ((t (:inherit font-lock-function-name-face :foreground "cornflower blue"))))))
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

