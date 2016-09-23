(provide 'wra-org)

;; allow alphabetical lists, like a), b) ... or a., b. ...
(setq org-list-allow-alphabetical t)

;; hide markup for bold and italic text
(setq org-hide-emphasis-markers t)

;; set agenda files
(setq org-agenda-files '("~/wiki/inf/comsys"))

;; org-mode
(require 'org)

;; turn on cdlatex in org mode
(add-hook 'org-mode-hook 'turn-on-org-cdlatex)

;; enable yasnippet
(add-hook 'org-mode-hook #'yas-minor-mode)

;; enable evaluation of code blocks for specific languages
(org-babel-do-load-languages
 'org-babel-load-languages
 '((emacs-lisp . t)
   (sh . t)))

;; latex math fragments preview
;;(setq org-latex-create-formula-image-program 'dvipng)
(setq org-latex-create-formula-image-program 'imagemagick)

;; additional packages
(add-to-list 'org-latex-packages-alist '("" "wraorgpreview" t))
;;(add-to-list 'org-latex-packages-alist '("" "mathrsfs" t))

;; translator orgtbl -> java array
(defun orgtbl-to-java-array (table params)
  "Convert the orgtbl-mode TABLE to a java array."
  (orgtbl-to-generic
   table
   (org-combine-plists
	'(:splice t :lstart "{" :lend "}," :llend "}" :sep ",")
	params)))

;; functions to publish my cs_wiki
(require 'ox-publish)

(setq org-publish-project-alist
	  '(
		("org-wra-cs-wiki"
		 ;; Path to your org files.
		 :base-directory "~/wiki/cs_wiki/org/"
		 :base-extension "org"

		 ;; Path to your Jekyll project.
		 :publishing-directory "~/wiki/cs_wiki/"
		 :recursive t
		 :publishing-function org-html-publish-to-html
		 :headline-levels 4 
		 :html-extension "html"
		 :body-only t ;; Only export section between <body> </body>
		 )


		("org-static-wra-cs-wiki"
		 :base-directory "~/wiki/cs_wiki/org/"
		 :base-extension "css\\|js\\|png\\|jpg\\|gif\\|pdf\\|mp3\\|ogg\\|swf\\|php\\|java\\|hs\\|erl"
		 :publishing-directory "~/wiki/cs_wiki/"
		 :recursive t
		 :publishing-function org-publish-attachment)

		("cs-wiki" :components ("org-wra-cs-wiki" "org-static-wra-cs-wiki"))

		))
