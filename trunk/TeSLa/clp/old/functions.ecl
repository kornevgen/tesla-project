:- module( functions ).
:- lib( ic ).
:- use_module( numbers ).

:- export 'sign_extend'/4.

% sign_extend(x) = ( x[31]^32 ).x
%% не сделана проверка соответствия X и SizeOfX !!!
'sign_extend2'( 
	number{ high: H, low: L }, 64,
	number{ high: 0, low: L }, 32
) :-
	D31 = 2147483648, D32M1 = 4294967295,
	( L #>= D31, H = D32M1 ; L #< D31, H = 0 ).

% sign_extend(x, M, N) = ( x[M-1]^(N-M) ).x[M-1..0]
% M - old length
% N - new length
% N > M
'sign_extend'( 
	number{ high: H, low: L },
	number{ high: H1, low: L1 }, M, N
) :-
	N #< 70, M #> 0,
	N #> M,
	
	MM1 #= M - 1,
	numbers:getbit( number( 0, Y1 ), number( H1, L1 ), MM1 ),
	
	NMM #= N - M,
	numbers:power( number( X2, Y2 ), NMM, number( 0, Y1 ), 1, NMM ),
	
	numbers:getbits( number( X3, Y3 ), M, number( H1, L1 ), MM1, 0 ),
	
	numbers:concat( number( H, L ), N, number( X2, Y2 ), NMM, number( X3, Y3 ), M ).
	