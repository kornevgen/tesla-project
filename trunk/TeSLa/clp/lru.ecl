:- module( lru ).

:- lib( ic ).

:- export setConditions/3.

setConditions( FreeCount, Universum, Result ) :-
	length( Universum, U ),
	U >= FreeCount, FreeCount >= 0,
	alldifferent( Universum ),

	% Free - набор индексов
	length( Free, FreeCount ),
	Free #:: [ 1 .. U ],
	ic_global:ordered( <, Free ),
	labeling( Free ),
	
	length( RFree, FreeCount ),
	setConds( Free, 1, Universum, R ),
	append( RFree, R, Result ).

setConds( [], _, R, R ) .
setConds( [ I | Ftail ], I, [ _ | Utail ], R ) :-
	I1 is I + 1,
	setConds( Ftail, I1, Utail, R ) .
setConds( [ F | Ftail ], I, [ U1 | Utail ], [ U1 | Rtail ] ) :-
	F > I,
	I1 is I + 1,
	setConds( [ F | Ftail ], I1, Utail, Rtail ) .