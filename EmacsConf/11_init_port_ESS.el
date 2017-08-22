;;; ====== 11_init_port_ESS ======
;;; aggiunto da Otto 18 feb 2009
(require 'ess-site) ;funzionava con ess 5.3.1 e rifunziona con 5.5.1
;; (load "/home/ottorino/ess/ess-5.5/lisp/ess-site")
;; added by otto on 6 09 2011
;; as from https://stat.ethz.ch/pipermail/ess-help/2011-March/006717.html
;; se commenti la riga sotto continua a chiedere di uccidere un processo
(setq ess-swv-pdflatex-commands '("pdflatex" "make"))
;; mette la spunta a Auctex in noweb
;; https://stat.ethz.ch/pipermail/ess-help/2011-February/006672.html
(setq ess-swv-plug-into-AUCTeX-p t)
; added by otto on 23 06 2009 
; http://www.xemacs.org/Documentation/packages/html/ess_3.html
(setq ess-ask-for-ess-directory nil) ; Non chiede la dir di lavoro (nil)
;; (setq inferior-ess-same-window nil)
(setq ess-help-own-frame 'one) ; open another emacs frame with help pages in it
;;; (setq inferior-R-args "--no-restore --no-save")
;;; added on 04 08 2009.
;;; https://stat.ethz.ch/pipermail/ess-help/2009-June/005410.html
;;  (defun my-ess-options ()
;;  ;;  (ess-set-style 'C++) ;; tolto sperimentalmente
;;    (column-number-mode t)
;;    (add-hook 'local-write-file-hooks
;; 	     (lambda ()
;; 	       (ess-nuke-trailing-whitespace)))
;; ;; ESC bsp cancella una parola
;;    (define-key ess-mode-map [(meta backspace)] 'backward-kill-word)
;; ;; definisce commenta/scommenta come auctex in latex
;;    (define-key ess-mode-map [(control ?c) (?;)] 'comment-region)
;;    (define-key ess-mode-map [(control ?c) (?:)] 'uncomment-region)
;; ;; line added by 8rino on 16 Mar 2011
;; ;; Ctrl-TAB autocompleta i nomi nei file .R ed .Rnw
;;    (define-key ess-mode-map (kbd "<C-tab>") 'ess-complete-object-name) 
;;  )
;; (add-hook 'ess-mode-hook 'my-ess-options)
;;  (defun my-iess-keybindings ()
;;    (define-key inferior-ess-mode-map [(control ?a)] 'comint-bol)
;;    (define-key inferior-ess-mode-map [home] 'comint-bol))
;;   (add-hook 'inferior-ess-mode-hook 'my-iess-keybindings)
;;; http://sites.google.com/site/andreaskiermeier/essmaterials
;;; added on 9 maggio 2011
;;; trovato http://thread.gmane.org/gmane.emacs.ess.general/2406
;;; e modificato di conseguenza
(add-to-list 'auto-mode-alist '("\\.Rnw\\'" . Rnw-mode))
(add-to-list 'auto-mode-alist '("\\.Snw\\'" . Snw-mode))
(add-hook 'Rnw-mode-hook
	  (lambda ()
	    (add-to-list 'TeX-expand-list  '("%rnw" file "Rnw" t) t)
	    (add-to-list 'TeX-command-list '("Stangle" "R CMD Stangle %rnw"
					     TeX-run-command nil (latex-mode) 
					     :help "Run Stangle") t)
	    (add-to-list 'TeX-command-list '("Sweave" "R CMD Sweave %rnw"
					     TeX-run-command nil (latex-mode) 
					     :help "Run Sweave") t)
					; '("Sweave" "R CMD Sweave %s" 
	    (add-to-list 'TeX-command-list '("LatexSweave" "%l \"%(mode)\\input{%s}\""
					; '("LatexSweave" "%l %(mode) %s"
					     TeX-run-TeX nil (latex-mode) 
					     :help "Run Latex after Sweave") t)
	    (setq TeX-command-default "Sweave")))
;;; ====== ALTER THE BEHAVIOUR OF SOME KEYS ====== 
;;; ====== Add a space before and after the "=" ====== 
(defun my-ess-equal-hook ()
  (local-set-key "=" (lambda () (interactive) (insert " = "))))
(add-hook 'ess-mode-hook 'my-ess-equal-hook)
(add-hook 'inferior-ess-mode-hook 'my-ess-equal-hook)
;; ====== Add a space before and after the "~" ====== 
(defun my-ess-tilde-hook ()
  (local-set-key "~" (lambda () (interactive) (insert " ~ "))))
(add-hook 'ess-mode-hook 'my-ess-tilde-hook)
(add-hook 'inferior-ess-mode-hook 'my-ess-tilde-hook)
;; ====== Add a space after the comma ====== 
;; (defun my-ess-comma-hook ()
;;  (local-set-key "," (lambda () (interactive) (insert ", "))))
;; (add-hook 'ess-mode-hook 'my-ess-comma-hook)
;; (add-hook 'inferior-ess-mode-hook 'my-ess-comma-hook)
;; 5 novembre 2014
;; http://blog.anghyflawn.net/2014/07/12/getting-ess-and-auctex-to-play-nicely-with-knitr/
;; somewhere after (require 'ess-site)
;; (defun ess-swv-add-TeX-commands ()
;;   "Add commands to AUCTeX's \\[TeX-command-list]."
;;   (unless (and (featurep 'tex-site) (featurep 'tex))
;;     (error "AUCTeX does not seem to be loaded"))
;;   (add-to-list 'TeX-command-list '("Knit" "Rscript -e \"library(knitr); knit('%t')\""
;; 				   TeX-run-command nil (latex-mode) :help
;; 				   "Run Knitr") t)
;;   (add-to-list 'TeX-command-list'("LaTeXKnit" "%l %(mode) %s"
;; 				  TeX-run-TeX nil (latex-mode) :help
;; 				  "Run LaTeX after Knit") t)
;;   (setq TeX-command-default "Knit")
;;   (mapc (lambda (suffix)
;;           (add-to-list 'TeX-file-extensions suffix))
;;         '("nw" "Snw" "Rnw")))
;; (defun ess-swv-remove-TeX-commands (x)
;;   "Helper function: check if car of X is one of the Knitr strings"
;;   (let ((swv-cmds '("Knit" "LaTeXKnit")))
;;     (unless (member (car x) swv-cmds) x)))
