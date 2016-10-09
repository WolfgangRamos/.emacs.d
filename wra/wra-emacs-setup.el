;;;; My Emacs startup file
;; author: Wolfgang Ramos
(provide 'wra-emacs-setup)


;; set home directory
(setq default-directory "/home/wra")





;;;; Apperance on Startup
;; inhibit start screen
(setq inhibit-startup-message t inhibit-startup-echo-area-message t)

;; inhibit tool-bar
(tool-bar-mode -1)

;; set default scratch mode to fundamental mode
(setq initial-major-mode 'fundamental-mode)


;;----------------------------------------------------------------------------;;
;; Modes                                                                      ;;
;;----------------------------------------------------------------------------;;

;; avy
(global-set-key (kbd "M-s") 'avy-goto-char)
(global-set-key (kbd "M-S") 'avy-goto-char-timer)
(global-set-key (kbd "M-l") 'avy-goto-line)

;; ace-window
(global-set-key (kbd "M-W") 'ace-window)

;; Whitespace-Mode (shows whitespaces)
(setq whitespace-style '(face tabs spaces newline indentation space-mark tab-mark newline-mark))

;; enable global hl-line-mode
(global-hl-line-mode nil)

;; global column-number-mode
(setq column-number-mode t)

;; Smart Tabs
(smart-tabs-insinuate 'java)

;; use imagex
(eval-after-load 'image '(require 'image+))

;; Helm
(require 'helm-config)
(helm-mode 1)
(define-key global-map [remap find-file] 'helm-find-files)
(define-key global-map [remap occur] 'helm-occur)
(define-key global-map [remap list-buffers] 'helm-buffers-list)
(define-key global-map [remap dabbrev-expand] 'helm-dabbrev)
(global-set-key (kbd "M-x") 'helm-M-x)
(unless (boundp 'completion-in-region-function)
  (define-key lisp-interaction-mode-map [remap completion-at-point] 'helm-lisp-completion-at-point)
  (define-key emacs-lisp-mode-map       [remap completion-at-point] 'helm-lisp-completion-at-point))
(add-hook 'kill-emacs-hook #'(lambda () (and (file-exists-p "/tmp/helm-cfg.el") (delete-file "/tmp/helm-cfg.el"))))

;; automatically resize suggestion window
(helm-autoresize-mode t)

;; Dired
(require 'wra-dired)

;; Recent files
(require 'recentf)

;; open recent files with C-x C-r
;; (global-set-key (kbd "C-x C-r") 'ido-recentf-open)
(global-set-key (kbd "C-x C-r") 'helm-recentf)

;; enable recent files mode.
(recentf-mode t)

;; 300 files ought to be enough.
(setq recentf-max-saved-items 300)

;; ispell
(require 'wra-ispell)

;; yasnippet
;; CAUTION: should be loaded before auto complete so that they can work together
(require 'wra-yasnippet)

;; cdlatex minor mode
(require 'wra-cdlatex)

;; org-mode
(require 'wra-org)

;; DocView Mode (pdf viewer)
;; (put 'set-goal-column 'disabled nil) ;; enable continuous scrolling

;; PDF Tools
(pdf-tools-install)
(add-hook 'doc-view-mode-hook
		  (lambda()
			(pdf-view-mode)))

;; Paren-Mode (shows matching brackets)
(setq show-paren-delay 0)

;; imaxima
(require 'wra-imaxima)

;; AucTeX
(require 'wra-auctex)

;; move line up/down with M-<up>/<down>
(require 'move-text)
(move-text-default-bindings)


;; Lisp
(add-hook 'emacs-lisp-mode-hook
	  (lambda ()
	    (show-paren-mode 1)
	    (whitespace-mode 1)))

	    
;; Haskell
(require 'haskell-interactive-mode)
(require 'haskell-process)
(add-hook 'haskell-mode-hook 'interactive-haskell-mode)
(add-hook 'haskell-mode-hook 'turn-on-haskell-indentation)
(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(doc-view-continuous t)
 '(haskell-process-auto-import-loaded-modules t)
 '(haskell-process-log t)
 '(haskell-process-suggest-remove-import-lines t))
(add-hook 'haskell-mode-hook
	  (lambda ()
	    column-number-mode 1))

;; Prolog (from: https://bruda.ca/emacs/prolog_mode_for_emacs)
(autoload 'run-prolog "prolog" "Start a Prolog sub-process." t)
(autoload 'prolog-mode "prolog" "Major mode for editing Prolog programs." t)
(autoload 'mercury-mode "prolog" "Major mode for editing Mercury programs." t)
(setq prolog-system 'swi)
(setq auto-mode-alist (append '(("\\.pl$" . prolog-mode)
                                ("\\.m$" . mercury-mode))
                               auto-mode-alist))


;; CC-Mode
(add-hook 'c-mode-hook
	  (lambda ()
	    (show-paren-mode 1)
	    (whitespace-mode 1)))



;; Java-mode
(add-hook 'java-mode-hook
	  (lambda ()
	    (show-paren-mode 1)
	    (whitespace-mode 1)))

(add-hook 'java-mode-hook #'yas-minor-mode)



;; Math preview
(autoload 'latex-math-preview-expression "latex-math-preview" nil t)
(autoload 'latex-math-preview-insert-symbol "latex-math-preview" nil t)
(autoload 'latex-math-preview-save-image-file "latex-math-preview" nil t)
(autoload 'latex-math-preview-beamer-frame "latex-math-preview" nil t)



;; Markdown-Mode
(autoload 'markdown-mode "markdown-mode"
   "Major mode for editing Markdown files" t)
(add-to-list 'auto-mode-alist '("\\.markdown\\'" . markdown-mode))
(add-to-list 'auto-mode-alist '("\\.md\\'" . markdown-mode))
(setq markdown-enable-math t) ;; enable math highlighting
(add-hook 'markdown-mode-hook
	  (lambda ()
	    (flyspell-mode-off)
	    (electric-indent-mode nil)
	    (setq indent-tabs-mode nil)))

(add-hook 'markdown-mode-hook #'yas-minor-mode)

;; unbind ⟨M-<UP>⟩ and ⟨M-<DOWN>⟩
(eval-after-load "markdown-mode"
  '(define-key markdown-mode-map (kbd "<M-down>") nil))
  ;;'(local-unset-key (kbd "<M-down>")))

(eval-after-load "markdown-mode"
  '(define-key markdown-mode-map (kbd "<M-up>") nil))
;;  '(local-unset-key (kbd "<M-up>")))

;; useful shortcuts:
;; S-TAB: cycle global visibility (in outline view)
;; TAB: cycle local visibility (in outline view)
;; C-c C-s s: make word at point bold
;; C-c C-s e: make word at point italic



;; pandoc (Minor) Mode
;; customize default pandoc settings file
;; pandoc-load-default-settings
;; setting directory for settings files
(setq pandoc-data-dir "~/cnf/emacs/pandoc_mode_settings_files/")
(add-hook 'pandoc-mode-hook 'pandoc-load-default-settings)



;; Magit
(setq magit-last-seen-setup-instructions "1.4.0")


;; make RET auto indent
(define-key global-map (kbd "RET") 'newline-and-indent)

;; make C-n insert new lines at the end of a file
(setq next-line-add-newlines t)

;; keybindings for movement by paragraph
(global-set-key (kbd "M-n") 'forward-paragraph)
(global-set-key (kbd "M-p") 'backward-paragraph)

;; Tabs
;; tab stops at: 4, 8, 12, ..., 80
;;(setq tab-stop-list (number-sequence 4 80 4))
;;(setq tab-width 4)
(setq-default tab-width 4)

;; always insert spaces instead of tabs
;;(setq-default indent-tabs-mode nil)


;; webkit
;;(require 'webkit)



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Customize Editing in Emacs
;;
;; the following commands and key bindings are defined
;; - ⟨C-x C-x⟩ exchange point and mark without selecting a region

;; exchange point and mark without selcting a region with C-x C-x
(defun exchange-point-and-mark-no-activate ()
  "Identical to \\[exchange-point-and-mark] but will not activate the region."
  (interactive)
  (exchange-point-and-mark)
  (deactivate-mark nil))
(define-key global-map [remap exchange-point-and-mark] 'exchange-point-and-mark-no-activate)

;; allow fast cycling throught the mark ring with: C-u C-Space, C-Space, ...
(setq set-mark-command-repeat-pop 1)

;; another way to jump to a specific mark: helm-mark-ring
(global-set-key (kbd "C-c C-SPC") 'helm-mark-ring)

;; duplicate line with C-<down> od C-<up>
(defun duplicate-line-down()
  (interactive)
  (move-beginning-of-line 1)
  (kill-line)
  (yank)
  (open-line 1)
  (next-line 1)
  (yank)
)
(global-set-key (kbd "<C-down>") 'duplicate-line-down)

(defun duplicate-line-up()
  (interactive)
  (move-beginning-of-line 1)
  (kill-line)
  (yank)
  (open-line 1)
  (next-line 1)
  (yank)
  (previous-line 1)
)
(global-set-key (kbd "<C-up>") 'duplicate-line-up)


;; toggle the case of a word with M-ö
;; -> taken from: http://ergoemacs.org/emacs/modernization_upcase-word.html
(defun toggle-letter-case ()
  "Toggle the letter case of current word or text selection.
Toggles between: “all lower”, “Init Caps”, “ALL CAPS”."
  (interactive)

  (let (p1 p2 (deactivate-mark nil) (case-fold-search nil))
    (if (use-region-p)
        (setq p1 (region-beginning) p2 (region-end))
      (let ((bds (bounds-of-thing-at-point 'word)))
        (setq p1 (car bds) p2 (cdr bds))))

    (when (not (eq last-command this-command))
      (save-excursion
        (goto-char p1)
        (cond
         ((looking-at "[[:lower:]][[:lower:]]") (put this-command 'state "all lower"))
         ((looking-at "[[:upper:]][[:upper:]]") (put this-command 'state "all caps"))
         ((looking-at "[[:upper:]][[:lower:]]") (put this-command 'state "init caps"))
         ((looking-at "[[:lower:]]") (put this-command 'state "all lower"))
         ((looking-at "[[:upper:]]") (put this-command 'state "all caps"))
         (t (put this-command 'state "all lower")))))

    (cond
     ((string= "all lower" (get this-command 'state))
      (upcase-initials-region p1 p2) (put this-command 'state "init caps"))
     ((string= "init caps" (get this-command 'state))
      (upcase-region p1 p2) (put this-command 'state "all caps"))
     ((string= "all caps" (get this-command 'state))
      (downcase-region p1 p2) (put this-command 'state "all lower")))))

;; bind function to a key
(global-set-key (kbd "M-ö") 'toggle-letter-case)


;; edit current file as root
(defun edit-current-file-as-root ()
  "Edit the file that is associated with the current buffer as root"
  (interactive)
  (let ((filep (buffer-file-name)))
    (if filep (find-file (concat "/sudo::" filep))
      (message "Current buffer does not have an associated file."))))


;; insert math right or left angle bracket
(defun insert-left-math-angle-bracket ()
  "Insert left mathematical angle bracket"
  (interactive)
  (insert-char 10216))

(defun insert-right-math-angle-bracket ()
  "Insert rigth mathematical angle bracket"
  (interactive)
  (insert-char 10217))


(global-set-key (kbd "C-(") 'insert-left-math-angle-bracket)
(global-set-key (kbd "C-)") 'insert-right-math-angle-bracket)



;; display ascii table in a buffer
  (defun ascii-table ()
    "Display basic ASCII table (0 thru 128)."
    (interactive)
    (switch-to-buffer "*ASCII*")
    (erase-buffer)
    (setq buffer-read-only nil)        ;; Not need to edit the content, just read mode (added)
    (local-set-key "q" 'bury-buffer)   ;; Nice to have the option to bury the buffer (added)
    (save-excursion (let ((i -1))
      (insert "ASCII characters 0 thru 127.\n\n")
      (insert " Hex  Dec  Char|  Hex  Dec  Char|  Hex  Dec  Char|  Hex  Dec  Char\n")
      (while (< i 31)
        (insert (format "%4x %4d %4s | %4x %4d %4s | %4x %4d %4s | %4x %4d %4s\n"
                        (setq i (+ 1  i)) i (single-key-description i)
                        (setq i (+ 32 i)) i (single-key-description i)
                        (setq i (+ 32 i)) i (single-key-description i)
                        (setq i (+ 32 i)) i (single-key-description i)))
        (setq i (- i 96))))))

;; configure shell buffer
(setq comint-scroll-to-bottom-on-input t)  ; always insert at the bottom
(setq comint-scroll-to-bottom-on-output nil) ; always add output at the bottom
(setq comint-scroll-show-maximum-output t) ; scroll to show max possible output


;; Emacs Speaks Shell
(require 'essh)
(defun essh-sh-hook ()
  (define-key sh-mode-map "\C-c\C-r" 'pipe-region-to-shell)
  (define-key sh-mode-map "\C-c\C-b" 'pipe-buffer-to-shell)
  (define-key sh-mode-map "\C-c\C-j" 'pipe-line-to-shell)
  (define-key sh-mode-map "\C-c\C-n" 'pipe-line-to-shell-and-step)
  (define-key sh-mode-map "\C-c\C-f" 'pipe-function-to-shell)
  (define-key sh-mode-map "\C-c\C-d" 'shell-cd-current-directory))
(add-hook 'sh-mode-hook 'essh-sh-hook)  


;; set default grep command
;;(grep-apply-setting 'grep-command "grep -Hnrwi \".\" -e ")
(setq grep-command "grep -Hnrwi \".\" -e ")
