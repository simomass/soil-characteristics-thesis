;;; ====== 15_init_port_misc ======
;; (add-to-list 'load-path "/home/ottorino/.emacs.d/Arduino")
;; (load "arduino-mode.el") ; added by 8rino
(load "folding" 'nomessage 'noerror)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; non funzionano su wheezy
;;(folding-mode-add-find-file-hook)
;;(folding-add-to-marks-list 'arduino-mode "\\{"  "\\}" nil t)
;; found in http://www.emacswiki.org/emacs/FoldingMode
;; https://github.com/bookest/arduino-mode/blob/master/arduino-mode.el
;; Da sperimentare al sab 10 ott 2015, 22.54.32, CEST
;; (require 'bbdb)
;; (bbdb-initialize)
;; #################### AVANZI VARI #############################
;;; -*- emacs-lisp -*-
;;; Fichier d'initialisation de GNU Emacs de Vincent Goulet
;;; ========================================================
;;; ====== Fichier de configuration externe ======
;; (setq custom-file
;;       (expand-file-name ".emacs-custom" "~"))
;; (load-file custom-file)
;;; ====== Associations ======
 ;; (setq auto-mode-alist
 ;;       (append '(("\\.inc$" . php-mode)
 ;;             ("\\.sql$" . sql-mode)
 ;;             ("\\.Rnw$" . Rnw-mode)
 ;;             ("\\.po\\'\\|\\.po\\." . po-mode))
 ;;           auto-mode-alist))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; disattivato su wheezy per il momento
;; http://www.sigmafield.org/2009/10/01/r-object-tooltips-in-ess
;(add-to-list 'load-path
;                   "/home/ottorino/.emacs.d/plugins/tooltips")
;(require 'ess-R-object-tooltip)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; disattivato su wheezy per il momento
;; http://www.svenhartenstein.de/Software/R-autoyas
;; (require 'r-autoyas);; nuova versione
; (load "/home/ottorino/.emacs.d/plugins/r-autoyas")
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; da qui in poi vecchia versione
 ;; (add-hook 'ess-mode-hook
 ;;           '(lambda ()
 ;;              (load "/home/ottorino/.emacs.d/plugins/r-autoyas.el")))
 ;;  (define-key ess-mode-map [67108904] 'r-autoyas-expand )
;; to make "(" automatically expand to yasnippets
;; (setq skeleton-pair t)
;; (setq skeleton-pair-alist
;;           '((?\( _ ?\))
;;             (?[  _ ?])
;;             (?{  _ ?})))
;; (define-key ess-mode-map (kbd "(") '(lambda () (interactive)
;;                                       (skeleton-pair-insert-maybe nil)
;;                                       (r-autoyas-expand nil t)))
;; fine autoyas
;;; fine della parte mia "classica-storica"
;;;ESS_ESS_ESS_ESS_ESS_ESS_ESS_ESS_ESS_ESS_ESS_ESS_ESS_ESS_ESS_ESS_ESS_ESS
;;  (defun Rnw-mode ()
;;   (require 'ess-noweb)
;; ;; from ESS 12.09 (noweb-mode) is obsolete
;; ;;http://grokbase.com/t/r/ess-help/12a15e7asy/ess-noweb-problem-after-upgrade-to-ess-12-09
;;   (ess-noweb-mode)
;;   (if (fboundp 'R-mode)
;;       (setq noweb-default-code-mode 'R-mode)))
;;   (add-hook 'ess-mode-hook 'my-ess-options)
;;   (add-hook 'inferior-ess-mode-hook 'my-iess-keybindings)
;;  (defun my-ess-options ()
;;    (ess-set-style 'C++)
;;    (column-number-mode t)
;;    (add-hook 'local-write-file-hooks
;;           (lambda ()
;;             (ess-nuke-trailing-whitespace)))
;;    (define-key ess-mode-map [(meta backspace)] 'backward-kill-word)
;; ;;   (define-key ess-mode-map [(control ?c) (?;)] 'comment-region)
;;    (define-key ess-mode-map [(control ?c) (?:)] 'uncomment-region)
;; ;; line added by 8rino on 16 Mar 2011
;;    (define-key ess-mode-map  (kbd "<C-tab>") 'ess-complete-object-name))
;;  (defun my-iess-keybindings ()
;;    (define-key inferior-ess-mode-map [(control ?a)] 'comint-bol)
;;    (define-key inferior-ess-mode-map [home] 'comint-bol))
;;; http://www.sigmafield.org/2009/10/01/r-object-tooltips-in-ess
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; disattivato per il momento su wheezy
;;; http://code.google.com/p/yasnippet/
;;: see also the file "/home/ottorino/.emacs.d/plugins/r-autoyas.el
;;; and /home/ottorino/.RProfile
; (add-to-list 'load-path
;                   "/home/ottorino/.emacs.d/plugins/yasnippet-0.6.1c")
;     (require 'yasnippet) ;; not yasnippet-bundle
;     (yas/initialize)
;     (yas/load-directory
;          "/home/ottorino/.emacs.d/plugins/yasnippet-0.6.1c/snippets")
;;; ====== TRAMP ======
;; (require 'tramp)
;; (setq tramp-default-method "scp")
;;; http://www.emacswiki.org/emacs/TrampMode
;;; ====== Fonction pour effacer la ligne courante ======
;; (defun kill-current-line ()
;;   "Deletes the current line"
;;   (interactive)
;;   (beginning-of-line)
;;   (kill-line)
;;   (kill-line))
;;; ====== Auto-compression ======
;; (auto-compression-mode nil)
;; (auto-compression-mode t)
;;; ====== Iswitch buffer ======
;; (iswitchb-mode t)
;;; http://www.emacswiki.org/emacs/IswitchBuffers
;;; dopo emacs 24 sostituito da IDO
;;; http://www.emacswiki.org/emacs/InteractivelyDoThings
;;; ====== Impression PostScript ======
;; (setq-default ps-print-header nil)
;;; ====== Effacer les blancs inutiles ======
;; (autoload 'nuke-trailing-whitespace "nuke-trailing-whitespace" nil t)
;; (setq nuke-trailing-whitespace-p t)
;;; sul portatile non funziona. non permette di salvare i file .R
;;; ====== Modifier timbre sonore ======
;; (setq ring-bell-function (lambda ()
;;                            (call-process "artsplay" nil 0 nil
;;                                       "/usr/share/sounds/KDE_Beep_Beep.wav")))
;;; ====== Ajout de répertoires à lisp-path ======
;; (setq load-path   (nconc '("/usr/local/share/emacs/site-lisp")
;;           load-path))
;; (setq exec-path
;;       (nconc '("/home/vincent/bin")
;;           exec-path))
;; (put 'narrow-to-region 'disabled nil)
