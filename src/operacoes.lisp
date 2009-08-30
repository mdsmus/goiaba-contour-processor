(in-package #:goiaba)
(use-package :pdf)

;; exemplo genérico de saída do goiaba
(let ((Z #s(0 5 3 4 1 3)))
  (simple-plot
   Z "original Z" :black
   (retrogradar Z) "retrograde" :gray
   (inverter Z) "inversion" :darkgray
   (rotacionar Z 1) "rotation" :lightgray))

(let ((Z (make-contorno-duracao-lista '((0 0) (1 5) (2 3) (3 4) (4 1)
                                         (5 3)))))
  (let ((*default-page-bounds* #(0 0 420 300)))
    (plot-page "/tmp/foo.pdf"
      (plot-contorno-full2 30 30
                           (list
                            Z "original Z" :black
                            (retrogradar Z) "retrograde" :gray
                            (inverter Z) "inversion" :darkgray
                            (rotacionar Z 1) "rotation" :lightgray)
                           :width 250 :height 250 :font-size 12.0
                           :grid-line-width .2 :grid-line-color :lightgrey
                           :line-width 2.5 :legend-font-size 20.0))))

;; exemplo sujeito do fugato
(let ((contorno #s(5 3 4 1 2 0)))
  (simple-plot
   contorno "P" :black
   (rotacionar contorno 3) "rot(P) 3" :gray))

(let ((contorno (make-contorno-duracao-lista '((0 5) (1 3) (2 4) (3 1) (4 2)
                                         (5 0)))))
  (let ((*default-page-bounds* #(0 0 420 300)))
    (plot-page "/tmp/foo.pdf"
      (plot-contorno-full2 30 30
                           (list
                            contorno "P" :black
                            (rotacionar contorno 3) "rot(P) 3" :gray)
                           :width 250 :height 250 :font-size 12.0
                           :grid-line-width .2 :grid-line-color :lightgrey
                           :line-width 2.5 :legend-font-size 20.0))))

;; exemplo contrasujeito do fugato
(let ((contorno #s(5 3 4 1 2 0)))
  (simple-plot
   (rotacionar (retrogradar contorno) 5) "rot(retr(P) 5)" :black
   (rotacionar (retrogradar contorno) 4) "rot(retr(P) 4)" :gray
   (rotacionar (retrogradar contorno) 3) "rot(retr(P) 3)" :lightgray))

(let ((contorno (make-contorno-duracao-lista '((0 5) (1 3) (2 4) (3 1) (4 2)
                                         (5 0)))))
  (let ((*default-page-bounds* #(0 0 440 300)))
    (plot-page "/tmp/foo.pdf"
      (plot-contorno-full2 30 30
                           (list
                            (rotacionar (retrogradar contorno) 5) "rot(retr(P) 5)" :black
                            (rotacionar (retrogradar contorno) 4) "rot(retr(P) 4)" :gray
                            (rotacionar (retrogradar contorno) 3) "rot(retr(P) 3)" :lightgray)
                           :width 250 :height 250 :font-size 12.0
                           :grid-line-width .2 :grid-line-color :lightgrey
                           :line-width 2.5 :legend-font-size 20.0))))

;;;;; com cor

;; exemplo genérico de saída do goiaba
(let ((Z (make-contorno-duracao-lista '((0 0) (1 5) (2 3) (3 4) (4 1)
                                         (5 3)))))
  (let ((*default-page-bounds* #(0 0 420 300)))
    (plot-page "/tmp/foo.pdf"
      (plot-contorno-full2 30 30
                           (list
                            Z "original Z" :black
                            (retrogradar Z) "retrograde" :green
                            (inverter Z) "inversion" :red
                            (rotacionar Z 1) "rotation" :blue)
                           :width 250 :height 250 :font-size 12.0
                           :grid-line-width .2 :grid-line-color :lightgrey
                           :line-width 2.5 :legend-font-size 20.0))))

;; exemplo sujeito do fugato
(let ((contorno (make-contorno-duracao-lista '((0 5) (1 3) (2 4) (3 1) (4 2)
                                         (5 0)))))
  (let ((*default-page-bounds* #(0 0 420 300)))
    (plot-page "/tmp/foo.pdf"
      (plot-contorno-full2 30 30
                           (list
                            contorno "P" :black
                            (rotacionar contorno 3) "rot(P) 3" :blue)
                           :width 250 :height 250 :font-size 12.0
                           :grid-line-width .2 :grid-line-color :lightgrey
                           :line-width 2.5 :legend-font-size 20.0))))

;; exemplo contrasujeito do fugato
(let ((contorno (make-contorno-duracao-lista '((0 5) (1 3) (2 4) (3 1) (4 2)
                                         (5 0)))))
  (let ((*default-page-bounds* #(0 0 440 300)))
    (plot-page "/tmp/foo.pdf"
      (plot-contorno-full2 30 30
                           (list
                            (rotacionar (retrogradar contorno) 5) "rot(retr(P) 5)" :black
                            (rotacionar (retrogradar contorno) 4) "rot(retr(P) 4)" :orange
                            (rotacionar (retrogradar contorno) 3) "rot(retr(P) 3)" :blue)
                           :width 250 :height 250 :font-size 12.0
                           :grid-line-width .2 :grid-line-color :lightgrey
                           :line-width 2.5 :legend-font-size 20.0))))
