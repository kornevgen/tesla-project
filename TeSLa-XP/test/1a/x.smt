(benchmark tesla
:logic QF_BV

:extrafuns ((x Int) (y1 Int) (y2 Int))

:assumption
(= y1 (ite (> x 1) 1 0) ) 

:assumption
(= y2 (ite (> x 1) 1 0) ) 

:assumption
(= x (+ y1 y2))

:assumption
(>= x 0)

)