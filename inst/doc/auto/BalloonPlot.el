(TeX-add-style-hook "BalloonPlot"
 (function
  (lambda ()
    (LaTeX-add-labels
     "figure:Figure1"
     "figure:Figure2"
     "figure:Figure3"
     "figure:Figure4")
    (TeX-run-style-hooks
     "graphicx"
     "array"
     "natbib"
     "round"
     "Rnews"
     "latex2e"
     "rep10"
     "report"
     "a4paper"))))

