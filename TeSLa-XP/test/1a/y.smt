(benchmark tesla
:logic QF_BV

:extrafuns ((x BitVec[5]))
:extrafuns ((y1 Int))
:extrafuns ((y2 Int))

:assumption
(and (= y1 (ite (= (extract[2:0] x) bv1[3]) 1 0) ) 
(= y2 (ite (= (extract[4:2] x) bv1[3]) 1 0) ) )

:assumption
(= 2 (+ y1 y2))

)