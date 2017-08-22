;; (defun uniquify-all-lines-region (start end)
;;     "Find duplicate lines in region START to END keeping first occurrence."
;;     (interactive "*r")
;;     (save-excursion
;;       (let ((end (copy-marker end)))
;;         (while
;;             (progn
;;               (goto-char start)
;;               (re-search-forward "^\\(.*\\)\n\\(\\(.*\n\\)*\\)\\1\n" end t))
;;           (replace-match "\\1\n\\2")))))

;;   (defun uniquify-all-lines-buffer ()
;;     "Delete duplicate lines in buffer and keep first occurrence."
;;     (interactive "*")
;;     (uniquify-all-lines-region (point-min) (point-max)))

;;; ====== 01_init_port_General ======
;;; ====== Stampante di default ======
(setq printer-name "HP_LaserJet_P2055dn")
;; (setq printer-name "Kyocera KM-2560")
;; (setq printer-name "hp:/net/HP_LaserJet_P2055dn?zc=NPI7CD54E")

;;; ====== Aspetto e funzionamento ======
(blink-cursor-mode t)                   ; blinking cursor (nil, not blinking)
(setq-default cursor-type 'bar)         ; C-h V display the kind of
                                        ; cursor (box, hollow, bar ...)
(set-face-background 'cursor "#CC0000") ; dark red cursor
(set-scroll-bar-mode 'right)            ; scroll bar on the right
(tool-bar-mode t )                      ; nil remove the menu bar, t shows it
(global-font-lock-mode t)               ; colorisation du texte
;; (transient-mark-mode t)              ; mode de sélection "normal"
(delete-selection-mode t)               ; overwrite the selected region
;; (setq-default mouse-yank-at-point t) ; se "t" incolla alla pos.del cursore
(show-paren-mode t)                     ; Show matching parentheses
;; (setq-default case-fold-search t)    ; recherche sans égard à la casse
;; (setq kill-whole-line t)             ; efface aussi les newlines
(setq default-major-mode 'text-mode)    ; mode par défaut
;; (set-language-environment "Latin-1") ; langage avec accents.
                                        ; Do not activate. In italian
                                        ; installation, accents disappears
                                        ; on activation (8rino)
(setq frame-title-format "%b")          ; mostra il nomefile in cima alla finestra
(global-unset-key (kbd "C-z"))          ; Disattiva il Ctrl-z, che minimizza emacs
(setq column-number-mode t)             ; Mostra anche il numero di colonna oltre alla riga
;;; http://superuser.com/questions/349943/how-to-awake-emacs-gui-after-pressing-ctrlz
;; http://www.emacswiki.org/emacs/JoelAdamson
(setq initial-frame-alist               ; Dimensioni del frame iniziale emacs
      '((top . 0) (left . 0)            ; parametri del frame iniziale
        (height . 54) (width . 80)))

;;; ====== Caratteri e colori ======
;; (set-face-font 'default "lucidasanstypewriter-12")
;; (set-face-font 'bold "lucidasans-bold-12")
;; (set-face-font 'italic "lucidasans-italic-12")
;; (set-face-font 'bold-italic "lucidasans-bolditalic-12")
;; (set-face-background 'mode-line "#EFEFEF")
(set-face-background 'region "lightskyblue1") ; color of the selection/region
;; (set-face-foreground 'font-lock-function-name-face "Blue3")
;; (set-face-foreground 'font-lock-keyword-face "Orange")
;; load-theme 'moe-dark)
;;(require 'moe-theme)
;; (moe-dark)
;; (moe-light)
     (add-to-list 'custom-theme-load-path "~/.emacs.d/elpa")
     (setq custom-safe-themes t)
     (load-theme 'adwaita t)

;;; ====== Repository di emacs ======
(require 'package)
(setq package-archives '(("gnu" . "http://elpa.gnu.org/packages/")
                         ("marmalade" . "https://marmalade-repo.org/packages/")
                         ("melpa" . "http://melpa.org/packages/")))

;;; ====== Salva la posizione all'interno del file ======
;; http://ergoemacs.org/emacs/emacs_save_cursor_position.html
;; turn on save place so that when opening a file, the cursor will be at the last position.
(require 'saveplace)
(setq save-place-file (concat user-emacs-directory "saveplace.el") ) ; use standard emacs dir
(setq-default save-place t)

;;; ====== Aggiunge una lista dei file piu' recenti ======
; stackoverflow.com/questions/50417/how-do-i-get-list-of-recent-files-in-gnu-emacs
(require 'recentf)
(recentf-mode 1)
(setq recentf-max-menu-items 25)
(global-set-key "\C-x\ \C-r" 'recentf-open-files)
;;; ====== Apre il gestore file dell'interfaccia grafica ======
;;; http://iwinuxfeed.altervista.org/aggregatore/emacs-aprire-il-file-manager-nella-cartelle-del-buffer-corrente-2/
(defun open-buffer-path ()
  "Run Nautilus on the directory of the current buffer." (interactive)
  (shell-command (concat "nemo " default-directory)))
(global-set-key (kbd "<M-f3>") 'open-buffer-path)

;;; ====== Aggiunge una funzione per contare le parole ======
;;; http://www.delorie.com/gnu/docs/emacs-lisp-intro/emacs-lisp-intro_208.html
(defun count-words-region (beginning end)
 "Restituisce il numero di parole nella regione."
 (interactive "r")
 (message "Sto contando ... ")
;;; 1. Set up appropriate conditions.
 (save-excursion
   (let ((count 0))
     (goto-char beginning)
;;; 2. Run the while loop.
     (while (and (< (point) end)
                 (re-search-forward "\\w+\\W*" end t))
       (setq count (1+ count)))
;;; 3. Send a message to the user.
     (cond ((zerop count)
            (message "La regione NON HA nessuna parola."))
           ((= 1 count)
            (message "La regione ha 1 parola."))
           (t
            (message "La regione ha %d parole." count))))))

;;; ====== Carica gli strumenti per le regular expressions ======
;;; http://newartisans.com/2007/10/a-regular-expression-ide-for-emacs/
(load "regex-tool" t)    ;; load regex-tool if it's available
;;; ====== comint-mode ======
;;  From Martin Maechler <maechler at stat.math.ethz.ch>:
;; (eval-after-load "comint" '(progn
;;   (setq comint-scroll-to-bottom-on-output 'others)
;;   (setq comint-scroll-show-maximum-output t)
   (setq comint-scroll-to-bottom-on-input t) ; Move to the end of the buffer,
                                             ; and place cursor on bottom line of window
;;   (define-key comint-mode-map [(meta ?p)] 'comint-previous-matching-input-from-input)
;;   (define-key comint-mode-map [(meta ?n)] 'comint-next-matching-input-from-input)
;;   (define-key comint-mode-map [(control ?a)] 'comint-bol-or-process-mark)
;;   (define-key comint-mode-map [home] 'comint-bol-or-process-mark)))
;; (add-hook 'comint-output-filter-functions 'comint-show-maximum-output nil t)
;;; ====== text-mode ======
(add-hook 'text-mode-hook 'turn-on-auto-fill)
;;; ====== Speedbar ======
(autoload 'speedbar-frame-mode "speedbar" "Popup a speedbar frame" t)
(autoload 'speedbar-get-focus "speedbar" "Jump to speedbar frame" t)
;;; ====== html-mode ======
;; (autoload 'sgml-mode "psgml" "Major mode to edit SGML documents" t)
;; (add-hook 'html-mode-hook 'my-html-mode)
;; (defun my-html-mode ()
;;   (setq ispell-extra-args '("-h"))
;;   (font-lock-mode)
;;   (font-lock-fontify-buffer)
;;   (auto-fill-mode))
;; (setq cssm-indent-function #'cssm-c-style-indenter)
;;; ====== cc-mode ======
;; (add-hook 'c-mode-common-hook 'my-cc-mode)
;; (defun my-cc-mode ()
;;tolti   (font-lock-mode)
;;tolti   (font-lock-fontify-buffer)
;;   (auto-fill-mode)
;;   (add-hook 'local-write-file-hooks
;;          (lambda ()
;;            (nuke-trailing-whitespace)))
;;   (c-toggle-auto-state)
;;   (c-toggle-hungry-state)
;;   (c-set-style "bsd")
;;   (setq c-basic-offset 4))
;;; ====== php-mode ======
;; (add-hook 'php-mode-hook 'my-cc-mode)
;;; ====== sql-mode ======
;; (setq sql-user "vincent")
;; (setq sql-server "localhost")
;; (setq sql-database "vgoulet")
;; (add-hook 'sql-mode-hook 'my-sql-keybindings)
;; (defun my-sql-keybindings ()
;;   (define-key sql-mode-map [(control ?a)] 'comint-bol)
;;   (define-key sql-mode-map [home] 'comint-bol))
;;; ====== po-mode ======
;; (autoload 'po-mode "po-mode" "Major mode for translators to edit PO files" t)
;;; ====== abbrev-mode ======
;; ;(setq-default abbrev-mode t)
;; ;(read-abbrev-file "~/.abbrevs")
;; ;(setq save-abbrev t)
;;; ====== Nouveaux keybindings globaux ======
;; (global-set-key [(meta ?z)] 'dup-line)
;; (global-set-key [(control ?x) (control ?k)] 'kill-current-line)
 (global-set-key [f4] 'speedbar-get-focus)
;; (global-set-key [f5] 'goto-line)
;; (global-set-key [f6] 'html-mode)
;; (global-set-key [f7] 'php-mode)
;; (global-set-key [f8] 'auto-fill-mode)
;; (global-set-key [f10] 'templ)
;; (global-set-key [f12] 'other-window)
;; (global-set-key [mouse-4] 'scroll-down)
;; (global-set-key [mouse-5] 'scroll-up)
;; (global-set-key [(meta delete)] 'kill-word)
;; ====== Aggiunte comode ======
;; indenta l'intero buffer
(defun indent-buffer ()
  (interactive)
  (indent-region (point-min) (point-max)))
