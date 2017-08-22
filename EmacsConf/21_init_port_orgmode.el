;;; ====== 21_init_port_orgmode ======
(require 'epa-file)
(epa-file-enable)
;;http://orgmode.org/worg/org-tutorials/orgtutorial_dto.html
(require 'org)
(require 'org-habit)
(define-key global-map "\C-cl" 'org-store-link)
(define-key global-map "\C-ca" 'org-agenda)
(define-key global-map "\C-cc" 'org-capture)
(setq org-log-done t)
(setq org-tag-alist '(("@lavoro" . ?l) ("@casa" . ?c) ("sogni" . ?s)))
;; include il diario di emacs
(setq org-agenda-include-diary t)
;; per contare il tempo speso nei vari progetti
(setq org-clock-persist 'history)
(org-clock-persistence-insinuate)
;; Http://mobileorg.ncogni.to/doc/getting-started/using-dropbox/
;; Set to the location of your Org files on your local system
 (setq org-directory
       "/mnt/D098970/org")
;; Set to <your Dropbox root directory>/MobileOrg.
(setq org-mobile-directory
      "~/Dropbox/Apps/MobileOrg")
;;(setq org-mobile-directory "davs://mydisk.com/ottorino/org")

;; Set to the name of the file where new notes will be stored
(setq org-mobile-inbox-for-pull
      "/mnt/D098970/org/DaSmartphone.org")

;; Nel file definito sopra, DaSmartphone.org, viene copiato il
;; contenuto del file ~/Dropbox/Apps/MobileOrg/mobileorg.org al
;; momento del pull da computer. PROVARE a usare il solo
;; ~/Dropbox/Apps/MobileOrg/mobileorg.org

;; definisce i files da mettere in agenda. If a
;; directory is part of this list, all files with the extension `.org' in
;; this directory will be part of the list.
(setq org-agenda-files
      (list
       "/mnt/D098970/org/lavoro.org"
       "/mnt/D098970/org/casa.org"
       "/mnt/D098970/org/Portare_a.org"
       "/mnt/D098970/org/SMART.org"
       "~/Documenti/BitBucket/lombrichi/Zanobi.org"
       ;; "~/Documenti/BitBucket/GIT_corso_vivaistica/2016_Vivaistica.org"
       ;; "/mnt/D098970/org/spazzatura.org"
       ;; "/mnt/D098970/dottorati2015/CorsoR.org"
       ))
;;(org-mobile-pull)
;;; per il refiling dei nodi (spesso dal flagged.org)
;;; http://stackoverflow.com/questions/22894492/refiling-to-new-node-in-org-mode
(setq org-refile-targets (quote ((nil :maxlevel . 10)
                                 (org-agenda-files :maxlevel . 10))))
(setq org-refile-use-outline-path t)
(setq org-outline-path-complete-in-steps nil)
(setq org-refile-allow-creating-parent-nodes (quote confirm))
;;  http://blog.everythingtastesbetterwithchilli.com/2013/02/10/org-mode-as-exocortex-introduction-to-mobile-org/
;; (setq org-mobile-files (cons (expand-file-name "DaSmartphone.org" org-directory)
;;                              org-agenda-files))
;; da http://osdir.com/ml/emacs-orgmode-gnu/2009-11/msg01138.html
(setq org-todo-keywords '(
                          (sequence "TODO(t)" "WAITING(w)" "DONE(d!)")
                          (type "PROJECT(p)" "CLOSED(c!)")
                          (sequence "ACQUISTA(a)" "ACQUISTATO(A!)"))
      )
;; da http://stackoverflow.com/questions/8751287/weekly-repeating-tasks-emacs-org-mode
;;(add-to-list 'org-modules "org-habit")
;; dalla pagina 167 del cartaceo, attiva R nelle parti di codice
;; pare cge funzioni sul portatile e non sul fisso
;; (org-babel-do-load-languages
;;  'org-babel-load-languages
;;  '((emacs-lisp . nil)
;;    (R . t)))
;; da http://orgmode.org/worg/org-faq.html
(setq org-deadline-warning-days 14)
(setq org-agenda-skip-scheduled-if-done t)
(setq org-agenda-skip-deadline-if-done t)
;;org-agenda-skip-timestamp-if-done
;; fine di http://orgmode.org/worg/org-faq.html
;; http://permalink.gmane.org/gmane.emacs.orgmode/92023
;; (with-temp-buffer
;;   (insert "* Spanish course
;; ** Spanish class
;; <2015-03-03 Tue 18:30-20:30>")
;;   (goto-line 2)
;;   (org-clone-subtree-with-time-shift 8 "+1w")
;;   (save-excursion (insert "** Spanish class
;; <2015-03-05 Thu 18:30-20:30>\n"))
;;   (org-clone-subtree-with-time-shift 7 "+1w")
;;   (goto-char (point-min)) (org-sort-entries nil ?t)
;;   ;; like C-c ^ t
;; (buffer-substring-no-properties (point-min) (point-max)))
;; http://stackoverflow.com/questions/19316917/have-basic-autoindentation-in-code-blocks-in-org-mode
(setq org-src-tab-acts-natively t)
;; http://orgmode.org/manual/Setting-up-capture.html#Setting-up-capture
(setq org-default-notes-file (concat org-directory "/DaSmartphone.org"))

;; questo sotto funziona con capture (C-c c)
;; http://stackoverflow.com/questions/24967910/org-mode-capture
;; (defun org-capture-class ()
;;   "Capture a class template for org-capture."
;;   (cl-labels ((update (date days)
;;                       (format-time-string
;;                        (car org-time-stamp-formats)
;;                        (seconds-to-time (+ (time-to-seconds date)
;;                                            (* days 86400))))))
;;     (let ((course   (read-string "Course: " nil nil '(nil)))
;;           (week     (read-string "Week: " nil nil '(nil)))
;;           (lecture  (read-string "Lecture No.: " nil nil '(nil)))
;;           (date     (org-read-date nil t))
;;           (location (read-string "Location: " nil nil '(nil))))
;;       (when (and course week lecture date location)
;;         (concat (format "* TODO %s: Week %s Lecture %s\n"
;;                         course week lecture)
;;                 (format "  SCHEDULED: %s\n" (update date 0))
;;                 (format "  Location: %s %%?\n" location)
;;                 (format "** TODO %s: prepare lecture %s from week %s\n"
;;                         course lecture week)
;;                 (format "   DEADLINE: %s SCHEDULED: %s\n"
;;                         (update date -1) (update date -2))
;;                 (format "** TODO %s: review lecture %s from week %s\n" course lecture week)
;;                 (format "   DEADLINE: %s SCHEDULED: %s\n" (update date 2) (update date 1)))))))
;; (setq org-capture-templates
;;    '(("c" "Class" entry
;;       (file "~/ottorino/Dropbox/org/class.txt")
;;       #'org-capture-class)
;;      ("e" "Exercise session" entry
;;       (file "~/ottorino/Dropbox/org/class.txt"))))
;; http://orgmode.org/worg/org-tutorials/org-custom-agenda-commands.html
;; https://groups.google.com/forum/#!topic/mobileorg-android/NMmV3Fop_Kc
(setq org-agenda-custom-commands
      '(("n" "Prossimo mese" agenda ""
         ((org-agenda-files '("/mnt/D098970/org/" ))
          ;;"/mnt/D098970/dottorati2015/CorsoR.org"
          (org-agenda-time-grid nil)
          (org-agenda-ndays 30)
          (org-agenda-entry-types '(:timestamp))))))
;;http://blog.everythingtastesbetterwithchilli.com/2013/02/10/org-mode-as-exocortex-introduction-to-mobile-org/
;;(org-mobile-pull) ;; run org-mobile-pull at startup
;; (defun install-monitor (file secs)
;;   (run-with-timer
;;    0 secs
;;    (lambda (f p)
;;      (unless (< p (second (time-since (elt (file-attributes f) 5))))
;;        (org-mobile-pull)))
;;    file secs))
;; (install-monitor (file-truename
;;                   (concat
;;                    (file-name-as-directory org-mobile-directory)
;;                           org-mobile-capture-file))
;;                  5)
;; ;; Do a pull every 5 minutes to circumvent problems with timestamping
;; ;; (ie. dropbox bugs)
;; (run-with-timer 0 (* 5 60) 'org-mobile-pull)
;; (setq org-agenda-custom-commands
;;       '(("c" "Desk Work" tags-todo "computer" ;; (1) (2) (3) (4)
;;       ((org-agenda-files '("~/org/widgets.org" "~/org/clients.org")) ;; (5)
;;        (org-agenda-sorting-strategy '(priority-up effort-down))
;;        ) ;; (5) cont.
;;       ("~/computer.html")) ;; (6)
;;      ;; ...other commands here
;;      ))
;;http://blog.andychase.net/posts/2010/08/emacs-org-mode-custom-agenda-filters-date-and-tag
;; (setq org-agenda-custom-commands
;;       `(("c" agenda "Chores"
;;      ((org-agenda-ndays 1)
;;       (org-scheduled-past-days 0)
;;       (org-deadline-warning-days 0)
;;       (org-agenda-filter-preset '("+chores"))
;;       )
;;      )))
