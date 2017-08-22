;;; ====== 04_init_port_dizionari ======
;;; ====== Ispell ====== 
;;(require 'ispell) si lamenta che ha gia ispell a bordo
;; SIMO_SIMO_SIMO_SIMO_SIMO_SIMO_SIMO_SIMO_SIMO_SIMO_
; added for bug testing on ispell
;; (setq ispell-local-dictionary "default")
;; (require 'ispell) ;; gia' caricato prima
(setq-default ispell-program-name "aspell")
(setq ispell-dictionary "italian")
(global-set-key '[(control ?c) (?i) (?d)] 'my-ispell-dictionary-default)
(global-set-key '[(control ?c) (?i) (?e)] 'my-ispell-dictionary-english)
(global-set-key '[(control ?c) (?i) (?i)] 'my-ispell-dictionary-italian)
(defun my-ispell-dictionary-default ()
  "Set default ispell dictionary" (interactive)
  (ispell-change-dictionary "default"))
(defun my-ispell-dictionary-english ()
  "Set english ispell dictionary" (interactive)
  (ispell-change-dictionary "am"))
(defun my-ispell-dictionary-italian ()
  "Set italian-tex ispell dictionary" (interactive)
  (ispell-change-dictionary "italian"))
(setq ispell-extra-args '("-t"))
;;; ===== Flyspell ======
;; http://www.emacswiki.org/emacs/FlySpell
;; easy spell check
(global-set-key (kbd "<f8>") 'ispell-word)
(global-set-key (kbd "C-S-<f8>") 'flyspell-mode)
(global-set-key (kbd "C-M-<f8>") 'flyspell-buffer)
(global-set-key (kbd "C-<f8>") 'flyspell-check-previous-highlighted-word)
(defun flyspell-check-next-highlighted-word ()
  "Custom function to spell check next highlighted word"
  (interactive)(flyspell-goto-next-error)
  (ispell-word)
  )
(global-set-key (kbd "M-<f8>") 'flyspell-check-next-highlighted-word)
