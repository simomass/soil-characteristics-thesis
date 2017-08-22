(TeX-add-style-hook
 "Tesi_GIT"
 (lambda ()
   (TeX-add-to-alist 'LaTeX-provided-class-options
                     '(("book" "11pt" "a4paper" "openright" "titlepage" "final" "language=italian")))
   (TeX-run-style-hooks
    "latex2e"
    "book"
    "bk11")))

