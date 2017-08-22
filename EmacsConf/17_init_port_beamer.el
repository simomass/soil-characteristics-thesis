;;; ====== 17_init_port_beamer ======
;; http://lists.gnu.org/archive/html/auctex/2006-01/msg00023.html
(require 'cl) ;serve perchè "letf" è stato sostituito da cl-letf e non va
;; vedi
;;http://www.gnu.org/software/emacs/manual/html_node/cl/Modify-Macros.html#Modify-Macros
(eval-after-load "tex"
  '(TeX-add-style-hook "beamer" 'my-beamer-mode))
(setq TeX-region "regionsje")
(defun my-beamer-mode ()
  "My adds on for when in beamer."
  ;; when in a Beamer file I want to use pdflatex.
  ;; Thanks to Ralf Angeli for this.
  (TeX-PDF-mode 1)                      ;turn on PDF mode.
  ;; Tell reftex to treat \lecture and \frametitle as section commands
  ;; so that C-c = gives you a list of frametitles and you can easily
  ;; navigate around the list of frames.
  ;; If you change reftex-section-level, reftex needs to be reset so that
  ;; reftex-section-regexp is correctly remade.
  (require 'reftex)
  (set (make-local-variable 'reftex-section-levels)
       '(("lecture" . 1) ("frametitle" . 2)))
  (reftex-reset-mode)
  ;; add some extra functions.
  (define-key LaTeX-mode-map "\C-cf" 'beamer-template-frame)
  (define-key LaTeX-mode-map "\C-cb" 'beamer-template-block)
  (define-key LaTeX-mode-map "\C-\M-z" 'tex-frame)) 
;; originale era "x" e non "z
;; si attiva con C-M-x, come scritto sopra
(defun tex-frame ()
  "Run pdflatex on current frame.  
Frame must be declared as an environment." (interactive)
(let (beg)
  (save-excursion (search-backward "\\begin{frame}")
		  (setq beg (point))
		  (forward-char 1)
		  (LaTeX-find-matching-end)
		  (TeX-pin-region beg (point))
		  (letf (( (symbol-function 'TeX-command-query) (lambda (x) "LaTeX")))
		    (TeX-command-region))
		  )
  ))
(defun beamer-template-frame ()
  "Create a simple template and move point to after \\frametitle." (interactive)
  (LaTeX-environment-menu "frame")
  (insert "\\frametitle{}")
  (backward-char 1))
(defun beamer-template-block ()
  "Create a simple template and move point to after block." (interactive)
  (LaTeX-environment-menu "block")
  (backward-char 5)
  (insert "{titolo}"))
(fset 'onslide
      "\\only<beamer:0|handout:0>{}%fine slide xx")
(global-set-key (kbd "<f12>") 'onslide)
