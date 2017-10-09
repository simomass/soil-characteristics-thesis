(TeX-add-style-hook
 "modello"
 (lambda ()
   (TeX-add-to-alist 'LaTeX-provided-class-options
                     '(("beamer" "xcolor={usenames, table, x11names}" "handout" "10pt")))
   (TeX-run-style-hooks
    "latex2e"
    "beamer"
    "beamer10"
    "pgfpages")))

