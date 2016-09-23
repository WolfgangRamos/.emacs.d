(provide 'wra-yasnippet)

;; should be loaded before auto complete so that they can work together
(require 'yasnippet)
(require 'helm-c-yasnippet) ;; use helm interface
(setq helm-yas-space-match-any-greedy t) ;[default: nil]
(global-set-key (kbd "C-c y") 'helm-yas-complete)

;; snippets dirs (new snippets are stored in the first one)
(setq yas-snippet-dirs
	  '("~/.emacs.d/snippets"                 ;; personal snippets
		"~/.emacs.d/elpa/yasnippet-20150811.1222/snippets"         ;; the default collection that lives in the yasnippet directory
		))

;; i activate yasnippet on per-mode basis
;; use `(yas-global-mode 1)` to activate yasnippet in all modes
(yas-reload-all)
