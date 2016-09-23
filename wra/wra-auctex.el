(provide 'wra-auctex)

;; AucTeX
;;(require 'tex-site)
(load "auctex.el" nil t t)
(setq TeX-auto-save t)
(setq TeX-parse-self t)
;;(setq-default TeX-master nil) ;; for multi-file documents
(setq TeX-PDF-mode t) ;; .pdf statt .dvi per default:

;; use latexmk
;;(require 'auctex-latexmk)
;;(auctex-latexmk-setup)
(eval-after-load "tex"
  '(add-to-list 'TeX-command-list
				'("latexmk -lualatex" "latexmk -lualatex %s" TeX-run-command t t :help "run latexmk with option -lualatex") t))
(eval-after-load "tex"
  '(add-to-list 'TeX-command-list
				'("latexmk -pdf" "latexmk -pdf %s" TeX-run-command t t :help "run latexmk with option -pdf") t))
;; make
(add-hook 'TeX-mode-hook '(lambda () (setq TeX-command-default "lualatex (latexmk)")))

;; Reftex einflechten und laden
(setq reftex-plug-into-AUCTeX t)
;; (add-hook 'LaTeX-mode-hook 'turn-on-reftex)

(add-hook 'LaTeX-mode-hook
		  (lambda()
			(yas-minor-mode) ;; enable yasnippet
			(turn-on-font-lock) ;; syntax highlight
			(LaTeX-math-mode)
			(turn-on-reftex)
			(turn-on-cdlatex)
			(hl-line-mode)
			(show-paren-mode)))

;;(add-hook 'LaTeX-mode-hook 'turn-on-auto-fill)

;;Syntax Highlight
;;(add-hook 'LaTeX-mode-hook 'turn-on-font-lock)

;; Mathe Modus
;;(add-hook 'LaTeX-mode-hook 'LaTeX-math-mode)


;; Satzende ". " statt ". ". " für M-k: löschen bis Satzende usw.
(setq sentence-end "[.?!][]\"’)}]*\\($\\| \\| \\)[
;;]*") ;; Da ist ein "Newline in der Zeile!"
(setq sentence-end-double-space nil)

;;direkte Rechtschreib Korrektur:
;;(add-hook 'LaTeX-mode-hook 'flyspell-mode)
;; Nur benutzen falls Auctex > 11.81 mit preview-latex:
;;(load "preview-latex.el" nil t t)
;; Deutsche Rechtschreibung falls \usepackage{ngerman} oder german benutzt wird
;;(add-hook 'TeX-language-de-hook
;;(function (lambda () (ispell-change-dictionary "german8"))))

