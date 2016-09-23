(provide 'wra-ispell)

;; borrowed from http://blog.binchen.org/posts/what-s-the-best-spell-check-set-up-in-emacs.html
(setq ispell-program-name "aspell")
;;(setq-default ispell-programm-name "aspell")


;; TODO add german dictionary to extra-dicts
;;(setq ispell-dictionary "de")

(defun flyspell-detect-ispell-args (&optional RUN-TOGETHER)
  "if RUN-TOGETHER is true, spell check the CamelCase words"
  (let ((args (list "--sug-mode=ultra" "--lang=de_DE")))
	(if RUN-TOGETHER
		(setq args (append args '("--run-together" "--run-together-limit=16" "--run-together-min=2"))))
	args))

;; ispell-cmd-args is useless, it's the list of *extra* command line arguments we will append to the ispell process when ispell-send-string()
;; ispell-extra-args is the command arguments which will *always* be used when start ispell process
(setq ispell-extra-args (flyspell-detect-ispell-args t))


(defadvice ispell-word (around my-ispell-word activate)
  (let ((old-ispell-extra-args ispell-extra-args))
	(ispell-kill-ispell t)
	;; use emacs original arguments
	(setq ispell-extra-args (flyspell-detect-ispell-args))
	ad-do-it
	;; restore our own ispell arguments
	(setq ispell-extra-args old-ispell-extra-args)
	(ispell-kill-ispell t)
	))

(defadvice flyspell-auto-correct-word (around my-flyspell-auto-correct-word activate)
  (let ((old-ispell-extra-args ispell-extra-args))
    (ispell-kill-ispell t)
    ;; use emacs original arguments
    (setq ispell-extra-args (flyspell-detect-ispell-args))
    ad-do-it
    ;; restore our own ispell arguments
    (setq ispell-extra-args old-ispell-extra-args)
    (ispell-kill-ispell t)
    ))
    
;; Add auto spell-checking in comments for all programming language modes
;; if and only if there is enough memory
;; You can use prog-mode-hook instead.
;; (if (and (not *no-memory*) ispell-program-name)
;;   (dolist (hook '(lisp-mode-hook
;;                   emacs-lisp-mode-hook
;;                   scheme-mode-hook
;;                   clojure-mode-hook
;;                   ruby-mode-hook
;;                   yaml-mode
;;                   python-mode-hook
;;                   shell-mode-hook
;;                   php-mode-hook
;;                   css-mode-hook
;;                   haskell-mode-hook
;;                   caml-mode-hook
;;                   c++-mode-hook
;;                   c-mode-hook
;;                   lua-mode-hook
;;                   crontab-mode-hook
;;                   perl-mode-hook
;;                   tcl-mode-hook
;;                   js2-mode-hook))
;;     (add-hook hook 'flyspell-prog-mode)))

;; you can also use "M-x ispell-word" or hotkey "M-$". It pop up a multiple choice
;; @see http://frequal.com/Perspectives/EmacsTip03-FlyspellAutoCorrectWord.html
;; (global-set-key (kbd "C-c s") 'flyspell-auto-correct-word)

;; {{ avoid spell-checking doublon (double word) in certain major modes
(defvar flyspell-check-doublon t
  "Check doublon (double word) when calling `flyspell-highlight-incorrect-region'.")
 (make-variable-buffer-local 'flyspell-check-doublon)

(defadvice flyspell-highlight-incorrect-region (around flyspell-highlight-incorrect-region-hack activate)
  (if (or flyspell-check-doublon (not (eq 'doublon (ad-get-arg 2))))
	  ad-do-it))
;; }}

;; function to toggle dictionaries with C-ä
(defun my-toggle-dictionary ()
  "Toggle Dictionaries"
  (interactive)
  (if (boundp 'ispell-current-dictionary)
      (cond
       ((string= "de" ispell-current-dictionary)
	(ispell-change-dictionary "en")
	(message "changed ispell dictionary to: English"))
       ((string= "en" ispell-current-dictionary)
	(ispell-change-dictionary "de")
	(message "changed ispell dictionary to: German")))
    (progn
      (setq ispell-current-dictionary "de")
      (message "set ispell dictionary to: German"))))

(global-set-key (kbd "C-ä") 'my-toggle-dictionary)
