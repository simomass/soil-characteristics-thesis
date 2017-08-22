;;; ====== 19_init_port_chemfig ======
;; tentativi di macro per chemfig
;; http://ergoemacs.org/emacs/emacs_macro_example.html
;; http://ergoemacs.org/emacs/keyboard_shortcuts.html
;; http://www.gnu.org/software/emacs/manual/html_node/emacs/Keyboard-Macro-Step_002dEdit.html
(fset 'mchem
      "\\ce{ }")
(global-set-key (kbd "<f6>") 'mchem)
(fset 'mchemReaction
      "\\begin{align*}
      \\cee{
        NH4+ + H2O &-> NH3 + OH-\\\
$\\overset{sopra}{\\ce{H20}}$ &$\\underset{sotto}{\\ce{->}}$ H2O 
      }
    \\end{align*}")
(global-set-key (kbd "S-<f6>") 'mchemReaction)
(fset 'definesubmol
      "\\definesubmol{Nome}{comandi}% fine molecola")
(global-set-key (kbd "<f7>") 'definesubmol)
(fset 'chemfig
      "\\chemfig[][scale=0.6]{[:rotaz]comandi}")
(global-set-key (kbd "<f8>") 'chemfig)
(fset 'reazione
      "\\schemestart
      \\chemname{;
        \\chemfig[][scale=0.5]{[:-30]A}}{composto A}
      \\arrow{->[sopra][sotto]}[10,2]% angolo, lungh, shift pag 55
      \\chemname{;;
        \\chemfig[][scale=0.5]{[:0]B}}{composto B}
      \\schemestop")
(global-set-key (kbd "<f9>") 'reazione)
