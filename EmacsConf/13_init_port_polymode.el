;;; ====== 13_init_port_polymode ======
;;; ====== Polymode ======
;;; https://github.com/vspinu/polymode
;; se non funziona, controlla che non sia commentato in .emacs
(setq load-path
      (append '("/home/ottorino/Documenti/BitBucket/polymode/"  "/home/ottorino/Documenti/BitBucket/polymode/modes")
(require 'poly-R)
(require 'poly-markdown)
(require 'poly-noweb)
;; MARKDOWN
(add-to-list 'auto-mode-alist '("\\.md" . poly-markdown-mode))
;; R modes
(add-to-list 'auto-mode-alist '("\\.Snw" . poly-noweb+r-mode))
(add-to-list 'auto-mode-alist '("\\.Rnw" . poly-noweb+r-mode))
(add-to-list 'auto-mode-alist '("\\.Rmd" . poly-markdown+r-mode))
;; Modifica Vitalie Spinu antifreeze 
;; https://github.com/vspinu/polymode/issues/87
      (defun markdown-match-propertized-text (property last)
        "Match text with PROPERTY from point to LAST.
      Restore match data previously stored in PROPERTY."
        (let ((saved (get-text-property (point) property))
              pos)
          (unless saved
            (setq pos (next-single-char-property-change (point) property nil last))
            (setq saved (get-text-property pos property)))
          (when saved
            (set-match-data saved)
            ;; Step at least one character beyond point. Otherwise
            ;; `font-lock-fontify-keywords-region' infloops.
            (goto-char (min (1+ (max (match-end 0) (point)))
                            (point-max)))
            saved)))
