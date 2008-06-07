:- module( predicates ).
:- lib( ic ).
:- use_module( numbers ).

:- export 'WordValue_long'/2.
:- export 'SNaN_single'/1.
:- export 'QNaN_single'/1.
:- export 'SNaN_double'/1.
:- export 'QNaN_double'/1.

'WordValue_long'( number{ high: H, low: L }, 64 ) :-
	( L #< 2147483648,
		H = 0
	; L #>= 2147483648,
		H = 4294967297
	).

'SNaN_single'( X ) :-
	numbers:is_int( X ),
	numbers:getbits( 255, _, X, 30, 23 ),
	numbers:getbit( 0, X, 22 ),
	numbers:getbits( numbers(0, A), _, X, 22, 0),
	A #\= 0.

'QNaN_single'( X ) :-
	numbers:is_int( X ),
	numbers:getbits( 511, _, X, 30, 22 ).

'SNaN_double'( X ) :-
	numbers:is_long( X ),
	numbers:getbits( 4095, _, X, 62, 51 ),
	numbers:notequal( A, number(0, 0) ),
	numbers:getbits( A, _, X, 50, 0 ).

'QNaN_double'( X ) :-
	numbers:is_long( X ),
	numbers:getbits( 4095, _, X, 62, 51 ).