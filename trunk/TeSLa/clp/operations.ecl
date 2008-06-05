:- module( operations ).
:- lib( ic ).
:- lib( tentative ).
:- lib( ic_kernel ).
%:- export concat/6.
%:- export is_int/1.
%:- export is_long/1.
:- export getbitFromNumber/3.
:- export getbitsFromNumber/5.
:- export pow/5.
:- export random_result/1.
:- export toSigned/3.
:- export toUnsigned/3.
:- export exp2/2.

% Result := 2^X, X >= 0
exp2( Result, X ) :-
	( X #= 0, Result #= 1
	; ( X #= 1, Result #= 2
	  ; X #> 1,
	           ( X #=< 32, exp2XlessThan33( Result, X )
		   ; X #> 32, X1 #= X - 32, exp2XlessThan33( R, X1), Result #= 4294967296 * R
		   )
	  )
	).

exp2XlessThan33( 1, 0 ).
exp2XlessThan33( Result, X ) :-
	X #> 0, X #=< 32,
	integers([Result, X]),
	( X #= 2 * N,
		Result $< sqr(exp( 1e-10 + N * ln(2.0) ) + 1e-6) ,
		Result $> sqr(exp( -1e-10 + N * ln(2.0) ) - 1e-6)
	; X #= 2 * N + 1,
		X1 #= X - 1,
		exp2XlessThan33( R, X1 ),
		Result #= 2 * R	
	).


% Result := X div A,  A == 2^N
div2exp2( Result, X, A ) :-
%	0 #=< D, D #< A,
%	X #= Result * A + D. 
	integers([Result, X, A]), 
	Result $< X/A + 1, 
	Result $> X/A - 1, 
	D #= X - Result * A, 0 #=< D, D #< A .


% Result = младш.бит(Number / 2^Index)
getbitFromNumber2( Result, Number, SizeOfNumber, Index ) :-
	Result :: [0..1],
	Number #>= 0,
	Index #>= 0, 
	% Index #< SizeOfNumber,
	integers([Number, SizeOfNumber, Index]),
	exp2( A, Index ),

	( Index #= SizeOfNumber - 1,
		( Number #>= A, Result #= 1 ; Number #< A, Result #= 0 )
	; Index #< SizeOfNumber - 1,
		div2exp2( C, Number, A ),
		( C #= 2*_, Result #= 0 ; C#= 2*_ + 1, Result #= 1)
	).
	
getbitFromNumber( Result, Number, Index ) :-
	Result :: [0..1],
	Number #>= 0,
	Index #>= 0, 
	% Index #< SizeOfNumber,
	integers([Number, Index]),
	exp2( A, Index ),

	div2exp2( C, Number, A ),
	( C #= 2*_, Result #= 0 ; C#= 2*_ + 1, Result #= 1).
	
	
% A := 2^StartIndex
% C := Number/A  (Number >> StartIndex)
% D := 2^(EndIndex - StartIndex + 1)
% Result >= 0, Result < D, integers([Result]), C = _ * D + Result
getbitsFromNumber( Result, SizeOfResult, Number, EndIndex, StartIndex ) :-
	Number #>= 0,
	integers([Result, Number, EndIndex, StartIndex]),
	EndIndex #>= StartIndex,
	SizeOfResult #= EndIndex - StartIndex + 1,

	exp2( A, StartIndex ),
	div2exp2( C, Number, A ),

% D := 2^(EndIndex - StartIndex + 1)
	Pow #= EndIndex - StartIndex + 1,
	exp2( D, Pow ),

	Result #>= 0, Result #< D,
	integers([Result]), 
	C #= _ * D + Result .
		

is_int( X ) :- X #:: [ -2147483648 .. 2147483647 ] .
is_long( X ) :- X #:: [ -9223372036854775808 .. 9223372036854775807 ] .


%% now bitlen/2 is not used
%
%bitlen( 1, 0 ).
%bitlen( Result, X ) :-
%	R #< 2 * X, X #=< R, exp2( R, Result ). 
%%
	
% Result = X.Y
% соответствие переменных и их размеров не проверяется!!!!!
concat( Result, SizeOfResult, X, SizeOfX, Y, SizeOfY ) :-
	X #>= 0, Y #>= 0,
	SizeOfResult #= SizeOfX + SizeOfY,
	exp2( Power, SizeOfY ),
	Result #= ( X * Power ) + Y.


% Result = X.X.X.X...X (pow раз)
pow( Result, SizeOfResult, X, SizeOfX, N ) :-
	X #>= 0,
	SizeOfResult #= N * SizeOfX,

	exp2( N_SX, SizeOfResult ),
	D #= X * ( N_SX - 1 ),

	exp2( N_X, SizeOfX ),
	ac_eq(N_X1, N_X, - 1),


	div2exp2( Result, D, N_X1 ).
		

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
random_result( X ) :-
	get_domain(X, L),
	( compound(L), 
	random_element(L, Fs1),
	X #>= Fs1,
	get_min(X, X)
	; integer(L) )
.
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

% SX = 4 => [1000] <= X <= [1111] == X isin [-8, 16)
% 1 = [0001] -> 1 (X = 1, SX = 4)
% 9 = [1001] -> -7 (X = 9, SX = 4) (старший-знаковый- бит равен 1 => это отрицательное число)
% -7 = [1001] -> -7 (X = 9, SX = 4) (число уже знаковое)
toSigned( Xnew, X, SizeOfX ) :-
	S1 #= SizeOfX - 1,
	exp2( ExpS1, S1 ),
	exp2( Exp2S1, SizeOfX),
	X #< Exp2S1, X #>= - ExpS1,
	( X #>= ExpS1, ac_eq(Xnew, X,  - Exp2S1)
	; X #< ExpS1, ac_eq(Xnew, X, 0) ).

% SX = 4 => [1000] <= X <= [1111] == X isin [-8, 16)
% 1 = [0001] -> 1 (X = 1, SX = 4)
% -7 = [1001] -> 9 (X = -7, SX = 4) (число требует преобразования, т.к. число отрицательное)
% 5 = [0101] -> 5 (X = 5, SX = 4) (число уже в беззнаковом представлении)
toUnsigned( Xnew, X, SizeOfX ) :-
	exp2( Exp2S1, SizeOfX ),
	( X #>= 0, X #< Exp2S1, ac_eq(Xnew, X, 0)
	; X #< 0, S1 #= SizeOfX - 1, exp2( ExpS1, S1 ), X #> -ExpS1, ac_eq(Xnew, X, Exp2S1) ).
	
	
	
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

% Z = X * Y
mmul( Z, X, Y ) :-
	mmul_cycle( Z, X, Y ).
	
mmul_cycle( 0, _, 0 ) :- !.
mmul_cycle( Z, X, Y ) :-
	ac_eq( Y1, Y, -1 ),
	mmul_cycle( Z1, X, Y1 ),
	ac_eq( Z, Z1, X ).