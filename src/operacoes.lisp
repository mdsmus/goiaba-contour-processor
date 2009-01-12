(in-package #:goiaba)
(use-package :pdf)

;; exemplo genérico de saída do goiaba
(let ((Z #s(0 5 3 4 1 3)))
  (simple-plot
   Z "original Z" :black
   (retrogradar Z) "retrograde" :gray
   (inverter Z) "inversion" :darkgray
   (rotacionar Z 1) "rotation" :lightgray))

;; exemplo sujeito do fugato
(let ((contorno #s(5 3 4 1 2 0)))
  (simple-plot
   contorno "P" :black
   (rotacionar contorno 3) "rot(P) 3" :gray))

;; exemplo contrasujeito do fugato
(let ((contorno #s(5 3 4 1 2 0)))
  (simple-plot
   (rotacionar (retrogradar contorno) 5) "rot(retr(P) 5)" :black
   (rotacionar (retrogradar contorno) 4) "rot(retr(P) 4)" :gray
   (rotacionar (retrogradar contorno) 3) "rot(retr(P) 3)" :lightgray))

