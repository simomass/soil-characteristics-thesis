;;; ====== 09_init_port_autocomplete ======
;; roba di autocomplete dom 11 ott 2015, 00.24.40, CEST
(add-to-list 'load-path "~/home/ottorino/.emacs.d/elpa/auto-complete-20150618.1949")    ; This may not be appeared if you have already added.
(require 'auto-complete-config)
(add-to-list 'ac-dictionary-directories "~/home/ottorino/.emacs.d/elpa/auto-complete-20150618.1949/ac-dict")
(ac-config-default)
;; http://www.emacswiki.org/emacs/ESSAuto-complete
(define-key ac-completing-map (kbd "M-h") 'ac-quick-help)
(define-key ac-completing-map "\M-n" nil) ;; was ac-next
(define-key ac-completing-map "\M-p" nil) ;; was ac-previous
(define-key ac-completing-map "\M-," 'ac-next)
(define-key ac-completing-map "\M-k" 'ac-previous)
;; Completion keys
;; When selecting completion candidates the return key inserts the
;; selected candidate. This can be a problem any time completion
;; candidates are triggered and you want to insert a new line,
;; because pressing return will complete the selected candidate
;; rather than insert a new line as intended. One solution is to
;; remove the binding of ac-complete to the return key”
;;(define-key ac-completing-map "\r" nil)
;; Optionally, you may wish to bind ac-complete to tab:
;; (define-key ac-completing-map "\t" 'ac-complete)
;; With recent versions of auto-complete you may need to set
;; (define-key ac-completing-map [tab] 'ac-complete)
 (define-key ac-completing-map [return] nil)
; instead.
;; colori dei riquadri
(set-face-attribute 'ac-candidate-face nil   :background "#00222c" :foreground "light gray")
(set-face-attribute 'ac-selection-face nil   :background "SteelBlue4" :foreground "white")
(set-face-attribute 'popup-tip-face    nil   :background "#003A4E" :foreground "light gray")
;; You can play with AC settings to adjust the completion to your needs:
;; (setq 
      ;; ac-auto-show-menu 1
      ;; ac-candidate-limit nil
      ;; ac-delay 0.1
      ;; ac-disable-faces (quote (font-lock-comment-face font-lock-doc-face))
      ;; ac-ignore-case 'smart
      ;; ac-menu-height 10
      ;; ac-quick-help-delay 1.5
      ;; ac-quick-help-prefer-pos-tip t
      ;; ac-use-quick-help nil
;; )
