(package-initialize) ;; serve per org-mode


(load "~/.emacs.d/VECCHIO/01_init_port_general")
(load "~/.emacs.d/VECCHIO/03_init_port_tempo")
(load "~/.emacs.d/VECCHIO/05_init_port_latex")
(load "~/.emacs.d/VECCHIO/07_init_port_dizionari")
(load "~/.emacs.d/VECCHIO/09_init_port_autocomplete")
(load "~/.emacs.d/VECCHIO/11_init_port_ESS")
(load "~/.emacs.d/VECCHIO/13_init_port_polymode")
(load "~/.emacs.d/VECCHIO/15_init_port_misc")
(load "~/.emacs.d/VECCHIO/17_init_port_beamer")
(load "~/.emacs.d/VECCHIO/19_init_port_chemfig")
(load "~/.emacs.d/VECCHIO/21_init_port_orgmode")


;;; ================================================
;;; FROM now on, Emacs write here through "Save options"
;;; All the above (similar) lines are overwritten by these last ones
;;; ================================================

(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(ansi-color-faces-vector
   [default default default italic underline success warning error])
 '(ansi-color-names-vector
   ["#303030" "#ff4b4b" "#d7ff5f" "#fce94f" "#5fafd7" "#d18aff" "#afd7ff" "#c6c6c6"])
 '(custom-enabled-themes (quote (zenburn)))
 '(custom-safe-themes
   (quote
    ("11636897679ca534f0dec6f5e3cb12f28bf217a527755f6b9e744bd240ed47e1" "685a7460fdc4b8c38796234d3a96b3aacbe4fba739fb33b5d6d149051ce74a58" "d677ef584c6dfc0697901a44b885cc18e206f05114c8a3b7fde674fce6180879" "8aebf25556399b58091e533e455dd50a6a9cba958cc4ebb0aab175863c25b9a4" "a8245b7cc985a0610d71f9852e9f2767ad1b852c2bdea6f4aadc12cce9c4d6d0" "e09e360191d36c837f6285574cc6a61980ab842266fe3a61d6b85b644a48587c" default)))
 '(ess-pdf-viewer-pref "evince")
 '(fci-rule-color "#383838")
 '(nrepl-message-colors
   (quote
    ("#CC9393" "#DFAF8F" "#F0DFAF" "#7F9F7F" "#BFEBBF" "#93E0E3" "#94BFF3" "#DC8CC3")))
 '(show-paren-mode t)
 '(vc-annotate-background "#2B2B2B")
 '(vc-annotate-color-map
   (quote
    ((20 . "#BC8383")
     (40 . "#CC9393")
     (60 . "#DFAF8F")
     (80 . "#D0BF8F")
     (100 . "#E0CF9F")
     (120 . "#F0DFAF")
     (140 . "#5F7F5F")
     (160 . "#7F9F7F")
     (180 . "#8FB28F")
     (200 . "#9FC59F")
     (220 . "#AFD8AF")
     (240 . "#BFEBBF")
     (260 . "#93E0E3")
     (280 . "#6CA0A3")
     (300 . "#7CB8BB")
     (320 . "#8CD0D3")
     (340 . "#94BFF3")
     (360 . "#DC8CC3"))))
 '(vc-annotate-very-old-color "#DC8CC3"))
(setq inhibit-splash-screen t)


(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(default ((t (:family "monospace" :foundry "b&h" :slant normal :weight normal :height 115 :width normal)))))
