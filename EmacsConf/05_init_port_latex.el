;;; ====== 05_init_port_latex ======
;;; ====== LaTeX-mode ====== 
(require 'bib-cite)
(require 'font-latex)
(setq-default TeX-master nil) ; allow to work with master documents
(setq TeX-parse-self t)       ; take into account \documentclass 
                              ; and \usepackage when pressing C-c-C-e
(setq TeX-auto-save t)        ; write in the dir "auto" informations on files 
                              ; used by master document
(setq-default TeX-auto-parse-length 200)
(setq LaTeX-default-options "12pt")
(setq-default TeX-PDF-mode t); added by 8rino AUC\TeX manual pag 47 
					; compila direttamente in pdf
(setq TeX-output-view-style ; define the viewers for latex files
      '(("^dvi$" "^pstricks$\\|^pst-\\|^psfrag$" "%(o?)dvips %d -o && gv %f")
	("^dvi$" "." "evince %dS %d")  	
	;; ("^dvi$" "." "atril %dS %d")      
	("^pdf$" "." "evince -f %o") 	
	;; ("^pdf$" "." "atril %o %(outpage)")
	("^html?$" "." "firefox %o")))
;; (load "preview-latex.el" nil t t) ; added by 8rino from AUC\TeX manual
(add-hook 'LaTeX-mode-hook 'my-latex-mode)
(defun my-latex-mode ()
  (add-to-list 'TeX-command-list '("Sweave" "Sweave.sh -l %s" TeX-run-command t t 
				   :help "Sweave file and produce DVI") t)
  (latex-math-mode)
  (TeX-fold-mode t) ;; can hide certain parts (like footnotes, references 
  ;; etc.) not often edited
  (turn-on-reftex)  ;; allows RefTeX to interact with AUCTeX
  ;;  (set-face-font 'font-latex-italic-face "lucidasans-italic-12")
  ;;  (set-face-font 'font-latex-bold-face "lucidasans-bold-12")
  ;;  (define-key LaTeX-math-keymap [?` (?-)] 'LaTeX-math-bar)
  ;;  (define-key LaTeX-math-keymap [?` (?e)] 'LaTeX-math-varepsilon)
  (turn-on-auto-fill) ; Breaks automatically lines at 80 columns while-u-write
  )
(setq reftex-file-extensions
      '(("Snw" "Rnw" "nw" "tex" ".tex" ".ltx") ("bib" ".bib")))
(setq TeX-file-extensions
      '("Snw" "Rnw" "nw" "tex" "sty" "cls" "ltx" "texi" "texinfo"))
