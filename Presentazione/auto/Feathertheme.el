(TeX-add-style-hook
 "Feathertheme"
 (lambda ()
   (TeX-add-to-alist 'LaTeX-provided-class-options
                     '(("beamer" "10pt")))
   (TeX-add-to-alist 'LaTeX-provided-package-options
                     '(("inputenc" "utf8") ("babel" "italian") ("fontenc" "T1")))
   (TeX-run-style-hooks
    "latex2e"
    "beamer"
    "beamer10"
    "inputenc"
    "babel"
    "fontenc"
    "helvet"
    "pgfplots"
    "ragged2e"
    "ocg-p"
    "blindtext"
    "hyperref"
    "pgfplotstable"
    "siunitx"
    "placeins"
    "datetime")
   (TeX-add-symbols
    '("chref" 2))
   (LaTeX-add-labels
    "tab:RiassuntoDensitaCAmpo"
    "tab:anova del modello"
    "tab:RiassuntoDensitaSpinta"
    "tab:Anova densita per spinta"
    "tab:Riassunto densita spinta"))
 :latex)

