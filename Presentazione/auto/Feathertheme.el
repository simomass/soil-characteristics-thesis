(TeX-add-style-hook
 "Feathertheme"
 (lambda ()
   (TeX-add-to-alist 'LaTeX-provided-class-options
                     '(("beamer" "10pt")))
   (TeX-run-style-hooks
    "latex2e"
    "beamer"
    "beamer10")))

