;;;; Setup Package Management
;; enable Emacs package management with MELPA
(when (>= emacs-major-version 24)
  (require 'package)
  (package-initialize)
  (add-to-list 'package-archives '("melpa" . "http://melpa.milkbox.net/packages/") t))

;; directory for packages not on MELPA
;; add .emacs.d/lisp to load-path
(let ((default-directory "~/.emacs.d/lisp/"))   
  (normal-top-level-add-to-load-path '("."))
  (normal-top-level-add-subdirs-to-load-path))

  
;; installed packages from MELPA:
;; magit, auctex latex-preview-math, window-numbering, helm, move-text, yasnippet, haskell-mode, markdown-mode, pandoc-mode
;; company, company-math, company-auctex, lua-mode, smart-tabs-mode


;;;; TODO move general setup stuff from wra-emacs-setup.el to this file (init.el)
(add-to-list 'load-path (expand-file-name "wra" user-emacs-directory))
(require 'wra-emacs-setup)




(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(doc-view-continuous t)
 '(haskell-process-auto-import-loaded-modules t)
 '(haskell-process-log t)
 '(haskell-process-suggest-remove-import-lines t)
 '(safe-local-variable-values
   (quote
	((tangledir . "~/prj/nv_prog/codebsp/")
	 (epa-armor . t)))))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )
