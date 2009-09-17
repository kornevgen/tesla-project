(benchmark tesla
:logic QF_BV
:extrafuns (( tagset1 BitVec[31] ))
:assumption
(and true 
)
:extrafuns (( tagset2 BitVec[31] ))
:assumption
(and true 
(= bit0 (bvcomp tagset1 tagset2))
)
:extrafuns (( tagset3 BitVec[31] ))
:assumption
(and true 
(= bit0 (bvcomp tagset1 tagset3))
(= bit0 (bvcomp tagset2 tagset3))
)
:extrafuns (( tagset4 BitVec[31] ))
:assumption
(and true 
(= bit0 (bvcomp tagset1 tagset4))
(= bit0 (bvcomp tagset2 tagset4))
(= bit0 (bvcomp tagset3 tagset4))
)
:extrafuns (( tagset5 BitVec[31] ))
:assumption
(and true 
(= bit0 (bvcomp tagset1 tagset5))
(= bit0 (bvcomp tagset2 tagset5))
(= bit0 (bvcomp tagset3 tagset5))
(= bit0 (bvcomp tagset4 tagset5))
)
:extrafuns (( tagset6 BitVec[31] ))
:assumption
(and true 
(= bit0 (bvcomp tagset1 tagset6))
(= bit0 (bvcomp tagset2 tagset6))
(= bit0 (bvcomp tagset3 tagset6))
(= bit0 (bvcomp tagset4 tagset6))
(= bit0 (bvcomp tagset5 tagset6))
)
:extrafuns (( tagset7 BitVec[31] ))
:assumption
(and true 
(= bit0 (bvcomp tagset1 tagset7))
(= bit0 (bvcomp tagset2 tagset7))
(= bit0 (bvcomp tagset3 tagset7))
(= bit0 (bvcomp tagset4 tagset7))
(= bit0 (bvcomp tagset5 tagset7))
(= bit0 (bvcomp tagset6 tagset7))
)
:extrafuns (( tagset8 BitVec[31] ))
:assumption
(and true 
(= bit0 (bvcomp tagset1 tagset8))
(= bit0 (bvcomp tagset2 tagset8))
(= bit0 (bvcomp tagset3 tagset8))
(= bit0 (bvcomp tagset4 tagset8))
(= bit0 (bvcomp tagset5 tagset8))
(= bit0 (bvcomp tagset6 tagset8))
(= bit0 (bvcomp tagset7 tagset8))
)
:extrafuns (( tagset9 BitVec[31] ))
:assumption
(and true 
(= bit0 (bvcomp tagset1 tagset9))
(= bit0 (bvcomp tagset2 tagset9))
(= bit0 (bvcomp tagset3 tagset9))
(= bit0 (bvcomp tagset4 tagset9))
(= bit0 (bvcomp tagset5 tagset9))
(= bit0 (bvcomp tagset6 tagset9))
(= bit0 (bvcomp tagset7 tagset9))
(= bit0 (bvcomp tagset8 tagset9))
)
:extrafuns (( tagset10 BitVec[31] ))
:assumption
(and true 
(= bit0 (bvcomp tagset1 tagset10))
(= bit0 (bvcomp tagset2 tagset10))
(= bit0 (bvcomp tagset3 tagset10))
(= bit0 (bvcomp tagset4 tagset10))
(= bit0 (bvcomp tagset5 tagset10))
(= bit0 (bvcomp tagset6 tagset10))
(= bit0 (bvcomp tagset7 tagset10))
(= bit0 (bvcomp tagset8 tagset10))
(= bit0 (bvcomp tagset9 tagset10))
)
:extrafuns (( tagset11 BitVec[31] ))
:assumption
(or 
(= tagset1 tagset11)
(= tagset2 tagset11)
(= tagset3 tagset11)
(= tagset4 tagset11)
(= tagset5 tagset11)
(= tagset6 tagset11)
(= tagset7 tagset11)
(= tagset8 tagset11)
(= tagset9 tagset11)
(= tagset10 tagset11)
)
:assumption
(> 4 (+ 
(ite (and 
(= bit0 (bvcomp tagset1 tagset11))
(= bit0 (bvcomp tagset2 tagset11))
(= bit0 (bvcomp tagset3 tagset11))
(= bit0 (bvcomp tagset4 tagset11))
(= bit0 (bvcomp tagset5 tagset11))
(= bit0 (bvcomp tagset6 tagset11))
(= bit0 (bvcomp tagset7 tagset11))
(= bit0 (bvcomp tagset8 tagset11))
(= bit0 (bvcomp tagset9 tagset11))
(= bit0 (bvcomp tagset10 tagset11))
(= (extract[6:0] tagset1) (extract[6:0] tagset11))) 1 0 ) 
(ite (and 
(= bit0 (bvcomp tagset2 tagset11))
(= bit0 (bvcomp tagset3 tagset11))
(= bit0 (bvcomp tagset4 tagset11))
(= bit0 (bvcomp tagset5 tagset11))
(= bit0 (bvcomp tagset6 tagset11))
(= bit0 (bvcomp tagset7 tagset11))
(= bit0 (bvcomp tagset8 tagset11))
(= bit0 (bvcomp tagset9 tagset11))
(= bit0 (bvcomp tagset10 tagset11))
(= (extract[6:0] tagset2) (extract[6:0] tagset11))) 1 0 ) 
(ite (and 
(= bit0 (bvcomp tagset3 tagset11))
(= bit0 (bvcomp tagset4 tagset11))
(= bit0 (bvcomp tagset5 tagset11))
(= bit0 (bvcomp tagset6 tagset11))
(= bit0 (bvcomp tagset7 tagset11))
(= bit0 (bvcomp tagset8 tagset11))
(= bit0 (bvcomp tagset9 tagset11))
(= bit0 (bvcomp tagset10 tagset11))
(= (extract[6:0] tagset3) (extract[6:0] tagset11))) 1 0 ) 
(ite (and 
(= bit0 (bvcomp tagset4 tagset11))
(= bit0 (bvcomp tagset5 tagset11))
(= bit0 (bvcomp tagset6 tagset11))
(= bit0 (bvcomp tagset7 tagset11))
(= bit0 (bvcomp tagset8 tagset11))
(= bit0 (bvcomp tagset9 tagset11))
(= bit0 (bvcomp tagset10 tagset11))
(= (extract[6:0] tagset4) (extract[6:0] tagset11))) 1 0 ) 
(ite (and 
(= bit0 (bvcomp tagset5 tagset11))
(= bit0 (bvcomp tagset6 tagset11))
(= bit0 (bvcomp tagset7 tagset11))
(= bit0 (bvcomp tagset8 tagset11))
(= bit0 (bvcomp tagset9 tagset11))
(= bit0 (bvcomp tagset10 tagset11))
(= (extract[6:0] tagset5) (extract[6:0] tagset11))) 1 0 ) 
(ite (and 
(= bit0 (bvcomp tagset6 tagset11))
(= bit0 (bvcomp tagset7 tagset11))
(= bit0 (bvcomp tagset8 tagset11))
(= bit0 (bvcomp tagset9 tagset11))
(= bit0 (bvcomp tagset10 tagset11))
(= (extract[6:0] tagset6) (extract[6:0] tagset11))) 1 0 ) 
(ite (and 
(= bit0 (bvcomp tagset7 tagset11))
(= bit0 (bvcomp tagset8 tagset11))
(= bit0 (bvcomp tagset9 tagset11))
(= bit0 (bvcomp tagset10 tagset11))
(= (extract[6:0] tagset7) (extract[6:0] tagset11))) 1 0 ) 
(ite (and 
(= bit0 (bvcomp tagset8 tagset11))
(= bit0 (bvcomp tagset9 tagset11))
(= bit0 (bvcomp tagset10 tagset11))
(= (extract[6:0] tagset8) (extract[6:0] tagset11))) 1 0 ) 
(ite (and 
(= bit0 (bvcomp tagset9 tagset11))
(= bit0 (bvcomp tagset10 tagset11))
(= (extract[6:0] tagset9) (extract[6:0] tagset11))) 1 0 ) 
(ite (and 
(= bit0 (bvcomp tagset10 tagset11))
(= (extract[6:0] tagset10) (extract[6:0] tagset11))) 1 0 ) 
 ))
:assumption
(or (and (= (extract[30:7] tagset11) bv0[24])  true)(and (= (extract[30:7] tagset11) bv231[24])  true)(and (= (extract[30:7] tagset11) bv18[24])  true)(and (= (extract[30:7] tagset11) bv179[24])  true)(and (= (extract[30:7] tagset11) bv457[24]) (>= 0 (+ 0 )))(and (= (extract[30:7] tagset11) bv2046[24]) (>= -1 (+ 0 )))(and (= (extract[30:7] tagset11) bv2087[24]) (>= -2 (+ 0 )))(and (= (extract[30:7] tagset11) bv18[24])  true)(and (= (extract[30:7] tagset11) bv179[24])  true)(and (= (extract[30:7] tagset11) bv2478[24]) (>= -5 (+ 0 )))(and (= (extract[30:7] tagset11) bv135[24]) (>= -6 (+ 0 )))(and (= (extract[30:7] tagset11) bv187[24]) (>= -7 (+ 0 )))(and (= (extract[30:7] tagset11) bv784[24]) (>= -8 (+ 0 )))(and (= (extract[30:7] tagset11) bv4[24]) (>= -9 (+ 0 ))))
:extrafuns (( tagset16 BitVec[31] ))
:assumption
(or 
(= tagset1 tagset16)
(= tagset2 tagset16)
(= tagset3 tagset16)
(= tagset4 tagset16)
(= tagset5 tagset16)
(= tagset6 tagset16)
(= tagset7 tagset16)
(= tagset8 tagset16)
(= tagset9 tagset16)
(= tagset10 tagset16)
(= tagset11 tagset16)
)

:extrafuns (( b1 BitVec[4] ))
:assumption
(= b1 (ite (and 
(= bit0 (bvcomp tagset1 tagset16))
(= bit0 (bvcomp tagset2 tagset16))
(= bit0 (bvcomp tagset3 tagset16))
(= bit0 (bvcomp tagset4 tagset16))
(= bit0 (bvcomp tagset5 tagset16))
(= bit0 (bvcomp tagset6 tagset16))
(= bit0 (bvcomp tagset7 tagset16))
(= bit0 (bvcomp tagset8 tagset16))
(= bit0 (bvcomp tagset9 tagset16))
(= bit0 (bvcomp tagset10 tagset16))
(= bit0 (bvcomp tagset11 tagset16))
(= (extract[6:0] tagset1) (extract[6:0] tagset16))) bv1[4] bv0[4] )  )

:extrafuns (( b2 BitVec[4] ))
:assumption
(= b2 (ite (and 
(= bit0 (bvcomp tagset2 tagset16))
(= bit0 (bvcomp tagset3 tagset16))
(= bit0 (bvcomp tagset4 tagset16))
(= bit0 (bvcomp tagset5 tagset16))
(= bit0 (bvcomp tagset6 tagset16))
(= bit0 (bvcomp tagset7 tagset16))
(= bit0 (bvcomp tagset8 tagset16))
(= bit0 (bvcomp tagset9 tagset16))
(= bit0 (bvcomp tagset10 tagset16))
(= bit0 (bvcomp tagset11 tagset16))
(= (extract[6:0] tagset2) (extract[6:0] tagset16))) bv1[4] bv0[4] )  )

:extrafuns (( b3 BitVec[4] ))
:assumption
(= b3 (ite (and 
(= bit0 (bvcomp tagset3 tagset16))
(= bit0 (bvcomp tagset4 tagset16))
(= bit0 (bvcomp tagset5 tagset16))
(= bit0 (bvcomp tagset6 tagset16))
(= bit0 (bvcomp tagset7 tagset16))
(= bit0 (bvcomp tagset8 tagset16))
(= bit0 (bvcomp tagset9 tagset16))
(= bit0 (bvcomp tagset10 tagset16))
(= bit0 (bvcomp tagset11 tagset16))
(= (extract[6:0] tagset3) (extract[6:0] tagset16))) bv1[4] bv0[4] )  )

:extrafuns (( b4 BitVec[4] ))
:assumption
(= b4 (ite (and 
(= bit0 (bvcomp tagset4 tagset16))
(= bit0 (bvcomp tagset5 tagset16))
(= bit0 (bvcomp tagset6 tagset16))
(= bit0 (bvcomp tagset7 tagset16))
(= bit0 (bvcomp tagset8 tagset16))
(= bit0 (bvcomp tagset9 tagset16))
(= bit0 (bvcomp tagset10 tagset16))
(= bit0 (bvcomp tagset11 tagset16))
(= (extract[6:0] tagset4) (extract[6:0] tagset16))) bv1[4] bv0[4] )  )

:extrafuns (( b5 BitVec[4] ))
:assumption
(= b5 (ite (and 
(= bit0 (bvcomp tagset5 tagset16))
(= bit0 (bvcomp tagset6 tagset16))
(= bit0 (bvcomp tagset7 tagset16))
(= bit0 (bvcomp tagset8 tagset16))
(= bit0 (bvcomp tagset9 tagset16))
(= bit0 (bvcomp tagset10 tagset16))
(= bit0 (bvcomp tagset11 tagset16))
(= (extract[6:0] tagset5) (extract[6:0] tagset16))) bv1[4] bv0[4] )  )

:extrafuns (( b6 BitVec[4] ))
:assumption
(= b6 (ite (and 
(= bit0 (bvcomp tagset6 tagset16))
(= bit0 (bvcomp tagset7 tagset16))
(= bit0 (bvcomp tagset8 tagset16))
(= bit0 (bvcomp tagset9 tagset16))
(= bit0 (bvcomp tagset10 tagset16))
(= bit0 (bvcomp tagset11 tagset16))
(= (extract[6:0] tagset6) (extract[6:0] tagset16))) bv1[4] bv0[4] )  )

:extrafuns (( b7 BitVec[4] ))
:assumption
(= b7 (ite (and 
(= bit0 (bvcomp tagset7 tagset16))
(= bit0 (bvcomp tagset8 tagset16))
(= bit0 (bvcomp tagset9 tagset16))
(= bit0 (bvcomp tagset10 tagset16))
(= bit0 (bvcomp tagset11 tagset16))
(= (extract[6:0] tagset7) (extract[6:0] tagset16))) bv1[4] bv0[4] )  )

:extrafuns (( b8 BitVec[4] ))
:assumption
(= b8 (ite (and 
(= bit0 (bvcomp tagset8 tagset16))
(= bit0 (bvcomp tagset9 tagset16))
(= bit0 (bvcomp tagset10 tagset16))
(= bit0 (bvcomp tagset11 tagset16))
(= (extract[6:0] tagset8) (extract[6:0] tagset16))) bv1[4] bv0[4])  )

:extrafuns (( b9 BitVec[4] ))
:assumption
(= b9 (ite (and 
(= bit0 (bvcomp tagset9 tagset16))
(= bit0 (bvcomp tagset10 tagset16))
(= bit0 (bvcomp tagset11 tagset16))
(= (extract[6:0] tagset9) (extract[6:0] tagset16))) bv1[4] bv0[4] )  )

:extrafuns (( b10 BitVec[4] ))
:assumption
(= b10 (ite (and 
(= bit0 (bvcomp tagset10 tagset16))
(= bit0 (bvcomp tagset11 tagset16))
(= (extract[6:0] tagset10) (extract[6:0] tagset16))) bv1[4] bv0[4] )  )

:extrafuns (( b11 BitVec[4] ))
:assumption
(= b11
(ite (and 
(= bit0 (bvcomp tagset11 tagset16))
(= (extract[6:0] tagset11) (extract[6:0] tagset16))
(or 
(= tagset16 tagset1)
(= tagset16 tagset2)
(= tagset16 tagset3)
(= tagset16 tagset4)
(= tagset16 tagset5)
(= tagset16 tagset6)
(= tagset16 tagset7)
(= tagset16 tagset8)
(= tagset16 tagset9)
(= tagset16 tagset10)
(= tagset16 tagset11)
(= bit0 (bvcomp tagset11 tagset1)))
(or 
(= tagset16 tagset2)
(= tagset16 tagset3)
(= tagset16 tagset4)
(= tagset16 tagset5)
(= tagset16 tagset6)
(= tagset16 tagset7)
(= tagset16 tagset8)
(= tagset16 tagset9)
(= tagset16 tagset10)
(= tagset16 tagset11)
(= bit0 (bvcomp tagset11 tagset2)))
(or 
(= tagset16 tagset3)
(= tagset16 tagset4)
(= tagset16 tagset5)
(= tagset16 tagset6)
(= tagset16 tagset7)
(= tagset16 tagset8)
(= tagset16 tagset9)
(= tagset16 tagset10)
(= tagset16 tagset11)
(= bit0 (bvcomp tagset11 tagset3)))
(or 
(= tagset16 tagset4)
(= tagset16 tagset5)
(= tagset16 tagset6)
(= tagset16 tagset7)
(= tagset16 tagset8)
(= tagset16 tagset9)
(= tagset16 tagset10)
(= tagset16 tagset11)
(= bit0 (bvcomp tagset11 tagset4)))
(or 
(= tagset16 tagset5)
(= tagset16 tagset6)
(= tagset16 tagset7)
(= tagset16 tagset8)
(= tagset16 tagset9)
(= tagset16 tagset10)
(= tagset16 tagset11)
(= bit0 (bvcomp tagset11 tagset5)))
(or 
(= tagset16 tagset6)
(= tagset16 tagset7)
(= tagset16 tagset8)
(= tagset16 tagset9)
(= tagset16 tagset10)
(= tagset16 tagset11)
(= bit0 (bvcomp tagset11 tagset6)))
(or 
(= tagset16 tagset7)
(= tagset16 tagset8)
(= tagset16 tagset9)
(= tagset16 tagset10)
(= tagset16 tagset11)
(= bit0 (bvcomp tagset11 tagset7)))
(or 
(= tagset16 tagset8)
(= tagset16 tagset9)
(= tagset16 tagset10)
(= tagset16 tagset11)
(= bit0 (bvcomp tagset11 tagset8)))
(or 
(= tagset16 tagset9)
(= tagset16 tagset10)
(= tagset16 tagset11)
(= bit0 (bvcomp tagset11 tagset9)))
(or 
(= tagset16 tagset10)
(= tagset16 tagset11)
(= bit0 (bvcomp tagset11 tagset10)))
 ) bv1[4] bv0[4] )  
 )

:extrafuns ((b BitVec[4]))
(= b (bvadd b1 b2 b3 b4 b5 b6 b7 b8 b9 b10 b11))
:assumption
(or (= bv4[4] b) (bvult bv4[4] b))

:extrafuns ((x Int) (y1 Int) (y2 Int))

:assumption
(= y1 (ite (> x 1) 1 0) ) 

:assumption
(= y2 (ite (> x 1) 1 0) ) 

:assumption
(= x (+ y1 y2))

:assumption
(>= x 0)

:assumption
(or (and (= (extract[30:7] tagset16) bv0[24])  true)(and (= (extract[30:7] tagset16) bv231[24])  true)(and (= (extract[30:7] tagset16) bv18[24])  true)(and (= (extract[30:7] tagset16) bv179[24]) (>= 1 (+ 0  (ite (and (or )(= bit0 (bvcomp (extract[30:7] tagset11) (extract[30:7] tagset11) )) ) 1 0))))(and (= (extract[30:7] tagset16) bv457[24]) (>= 0 (+ 0  (ite (and (or )(= bit0 (bvcomp (extract[30:7] tagset11) (extract[30:7] tagset11) )) ) 1 0))))(and (= (extract[30:7] tagset16) bv2046[24]) (>= -1 (+ 0  (ite (and (or )(= bit0 (bvcomp (extract[30:7] tagset11) (extract[30:7] tagset11) )) ) 1 0))))(and (= (extract[30:7] tagset16) bv2087[24]) (>= -2 (+ 0  (ite (and (or )(= bit0 (bvcomp (extract[30:7] tagset11) (extract[30:7] tagset11) )) ) 1 0))))(and (= (extract[30:7] tagset16) bv18[24])  true)(and (= (extract[30:7] tagset16) bv179[24]) (>= 1 (+ 0  (ite (and (or )(= bit0 (bvcomp (extract[30:7] tagset11) (extract[30:7] tagset11) )) ) 1 0))))(and (= (extract[30:7] tagset16) bv2478[24]) (>= -5 (+ 0  (ite (and (or )(= bit0 (bvcomp (extract[30:7] tagset11) (extract[30:7] tagset11) )) ) 1 0))))(and (= (extract[30:7] tagset16) bv135[24]) (>= -6 (+ 0  (ite (and (or )(= bit0 (bvcomp (extract[30:7] tagset11) (extract[30:7] tagset11) )) ) 1 0))))(and (= (extract[30:7] tagset16) bv187[24]) (>= -7 (+ 0  (ite (and (or )(= bit0 (bvcomp (extract[30:7] tagset11) (extract[30:7] tagset11) )) ) 1 0))))(and (= (extract[30:7] tagset16) bv784[24]) (>= -8 (+ 0  (ite (and (or )(= bit0 (bvcomp (extract[30:7] tagset11) (extract[30:7] tagset11) )) ) 1 0))))(and (= (extract[30:7] tagset16) bv4[24]) (>= -9 (+ 0  (ite (and (or )(= bit0 (bvcomp (extract[30:7] tagset11) (extract[30:7] tagset11) )) ) 1 0)))) (= (extract[30:7] tagset16) (extract[30:7] tagset11)) )
:extrafuns (( x_X BitVec[64] ))
:extrafuns (( y_X BitVec[64] ))
:extrafuns (( c_X BitVec[16] ))
:extrafuns (( y_X_X BitVec[64] ))
:extrafuns (( _localvar_21 BitVec[64] ))
:assumption
(= _localvar_21 (bvadd (sign_extend[48] c_X)x_X))
:assumption
(= (extract[1:0] _localvar_21)bv0[2])
:extrafuns(( va12 BitVec[64] ))
:extrafuns(( pat13 BitVec[36] ))
:assumption
(= va12 _localvar_21)
:assumption
(= pat13 pat13)
:assumption
(= bv0[33] (extract[63:31] va12))
:assumption
(let (_localvar_22 (extract[30:7] tagset11))
(let (_localvar_23 (extract[39:12] va12))
(or 
(and (= _localvar_22 bv0[24])(= _localvar_23 bv167568[28]))
(and (= _localvar_22 bv18[24])(= _localvar_23 bv167569[28]))
(and (= _localvar_22 bv231[24])(= _localvar_23 bv115714[28]))
(and (= _localvar_22 bv179[24])(= _localvar_23 bv115715[28]))
(and (= _localvar_22 bv18[24])(= _localvar_23 bv58950[28]))
(and (= _localvar_22 bv2478[24])(= _localvar_23 bv58951[28]))
(and (= _localvar_22 bv179[24])(= _localvar_23 bv36950[28]))
(and (= _localvar_22 bv135[24])(= _localvar_23 bv36951[28]))
(and (= _localvar_22 bv457[24])(= _localvar_23 bv133468[28]))
(and (= _localvar_22 bv187[24])(= _localvar_23 bv133469[28]))
(and (= _localvar_22 bv2046[24])(= _localvar_23 bv51392[28]))
(and (= _localvar_22 bv784[24])(= _localvar_23 bv51393[28]))
(and (= _localvar_22 bv2087[24])(= _localvar_23 bv21192[28]))
(and (= _localvar_22 bv4[24])(= _localvar_23 bv21193[28]))
)))
:assumption
(= pat13 (concat (extract[30:7] tagset11) (extract[11:0] va12) ) )
:assumption
(= (extract[11:5] va12) (extract[6:0] tagset11))
:extrafuns (( _localvar_24 BitVec[3] ))
:assumption
(= _localvar_24 (extract[2:0] _localvar_21))
:extrafuns(( data15 BitVec[64] ))
:extrafuns(( _localvar_25 BitVec[33] ))
:assumption
(= data15 data15)
:assumption
(and true 
)
:extrafuns (( _localvar_26 BitVec[32] ))
:assumption
(ite (= bv0[3] _localvar_24) (= _localvar_26 (extract[31:0] data15))(= _localvar_26 (extract[63:32] data15)))
:assumption
(= y_X_X(sign_extend[32] _localvar_26))
:extrafuns (( _localvar_27 BitVec[64] ))
:assumption
(= _localvar_27 (bvadd (sign_extend[48] c_X)x_X))
:assumption
(= (extract[1:0] _localvar_27)bv0[2])
:extrafuns(( va17 BitVec[64] ))
:extrafuns(( pat18 BitVec[36] ))
:assumption
(= va17 _localvar_27)
:assumption
(= pat18 pat18)
:assumption
(= bv0[33] (extract[63:31] va17))
:assumption
(let (_localvar_28 (extract[30:7] tagset16))
(let (_localvar_29 (extract[39:12] va17))
(or 
(and (= _localvar_28 bv0[24])(= _localvar_29 bv167568[28]))
(and (= _localvar_28 bv18[24])(= _localvar_29 bv167569[28]))
(and (= _localvar_28 bv231[24])(= _localvar_29 bv115714[28]))
(and (= _localvar_28 bv179[24])(= _localvar_29 bv115715[28]))
(and (= _localvar_28 bv18[24])(= _localvar_29 bv58950[28]))
(and (= _localvar_28 bv2478[24])(= _localvar_29 bv58951[28]))
(and (= _localvar_28 bv179[24])(= _localvar_29 bv36950[28]))
(and (= _localvar_28 bv135[24])(= _localvar_29 bv36951[28]))
(and (= _localvar_28 bv457[24])(= _localvar_29 bv133468[28]))
(and (= _localvar_28 bv187[24])(= _localvar_29 bv133469[28]))
(and (= _localvar_28 bv2046[24])(= _localvar_29 bv51392[28]))
(and (= _localvar_28 bv784[24])(= _localvar_29 bv51393[28]))
(and (= _localvar_28 bv2087[24])(= _localvar_29 bv21192[28]))
(and (= _localvar_28 bv4[24])(= _localvar_29 bv21193[28]))
)))
:assumption
(= pat18 (concat (extract[30:7] tagset16) (extract[11:0] va17) ) )
:assumption
(= (extract[11:5] va17) (extract[6:0] tagset16))
:extrafuns (( _localvar_30 BitVec[3] ))
:assumption
(= _localvar_30 (extract[2:0] _localvar_27))
:extrafuns (( data20 BitVec[64] ))
:extrafuns (( _localvar_31 BitVec[64] ))
:assumption
(ite (= _localvar_30 bv0[3])(= _localvar_31 (concat (concat bv0[56] (extract[7:0] y_X_X)) bv0[0]))
(ite (= _localvar_30 bv1[3])(= _localvar_31 (concat (concat bv0[48] (extract[7:0] y_X_X)) bv0[8]))
(ite (= _localvar_30 bv2[3])(= _localvar_31 (concat (concat bv0[40] (extract[7:0] y_X_X)) bv0[16]))
(ite (= _localvar_30 bv3[3])(= _localvar_31 (concat (concat bv0[32] (extract[7:0] y_X_X)) bv0[24]))
(ite (= _localvar_30 bv4[3])(= _localvar_31 (concat (concat bv0[24] (extract[7:0] y_X_X)) bv0[32]))
(ite (= _localvar_30 bv5[3])(= _localvar_31 (concat (concat bv0[16] (extract[7:0] y_X_X)) bv0[40]))
(ite (= _localvar_30 bv6[3])(= _localvar_31 (concat (concat bv0[8] (extract[7:0] y_X_X)) bv0[48]))
(ite (= _localvar_30 bv7[3])(= _localvar_31 (concat (concat bv0[0] (extract[7:0] y_X_X)) bv0[56]))
false
))))))))
:extrafuns(( _localvar_32 BitVec[33] ))
:assumption
(= data20 _localvar_31)
)
