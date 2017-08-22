;;; ====== 03_init_port_TempoDate ======

; ---- language-env DON'T MODIFY THIS LINE!
;; Names for calendar command.
;; These should be derived from nl_langinfo() by emacs
;;
    (setq calendar-week-start-day 1
           calendar-day-name-array ["Domenica" "Lunedì" "Martedì" "Mercoledì" "Giovedì" "Venerdì" "Sabato"]
           calendar-month-name-array ["Gennaio" "Febbraio" "Marzo" "Aprile" "Maggio" "Giugno" "Luglio" "Agosto" "Settembre" "Ottobre" "Novembre" "Dicembre"]
           calendar-day-header-array ["Do" "Lu" "Ma" "Me" "Gi" "Ve" "Sa"]
           )
; ---- language-env end DON'T MODIFY THIS LINE!
;;; ======= Vacanze italiane ======
;; http://www.emacswiki.org/emacs/CalendarLocalization#toc34
     (setq holiday-general-holidays
           '((holiday-fixed 1 1 "Capodanno")
             (holiday-fixed 5 1 "1 Maggio")
             (holiday-fixed 4 25 "Liberazione")
             (holiday-fixed 6 2 "Festa Repubblica")))
     (setq holiday-christian-holidays
           '((holiday-fixed 12 8 "Immacolata Concezione")
             (holiday-fixed 12 25 "Natale")
             (holiday-fixed 12 26 "Santo Stefano")
             (holiday-fixed 1 6 "Epifania")
             (holiday-easter-etc -52 "Giovedì grasso")
             (holiday-easter-etc -47 "Martedì grasso")
             (holiday-easter-etc  -2 "Venerdì Santo")
             (holiday-easter-etc   0 "Pasqua")
             (holiday-easter-etc  +1 "Lunedì Pasqua")
             (holiday-fixed 8 15 "Assunzione di Maria")
             (holiday-fixed 11 1 "Ognissanti")))
     ;; universita
     (setq holiday-other-holidays
           '((holiday-fixed 1 4 "Università chiusa")
             (holiday-fixed 1 5 "Università chiusa")
             (holiday-fixed 6 3 "Università chiusa")
             (holiday-fixed 8 16 "Università chiusa")
             (holiday-fixed 8 17 "Università chiusa")
             (holiday-fixed 8 18 "Università chiusa")
             (holiday-fixed 8 19 "Università chiusa")
             (holiday-fixed 10 31 "Università chiusa")       
             (holiday-fixed 12 9 "Università chiusa")))
     (setq calendar-holidays
           (append general-holidays local-holidays other-holidays
                   christian-holidays))
;;; ====== Coordinate geografiche ======
;;http://www.gnu.org/software/emacs/manual/html_node/emacs/Sunrise_002fSunset.html
(setq calendar-latitude 43.833333)
(setq calendar-longitude 11.333333)
(setq calendar-location-name "Firenze, Italia")
