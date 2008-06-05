:- module( operations ).
:- lib( ic ).
:- lib( tentative ).
:- lib( ic_kernel ).
:- export concat/3.
:- export is_int/1.
:- export is_long/1.
:- export getbit/3.
:- export getbits/4.
:- export pow/3.
:- export random_result/1.

% Result := 2^X
exp21( Result, X ) :-
	integers([Result, X]),
	ln(A_real0) $= X * ln(2.0),
%% погрешность при взятии степени берем равной 0.1
	A_real $= A_real0 + 0.1,
	integers([Result]),
	Result $=< A_real,
	A_real $< Result + 1.

%exp2( 1, 0 ).
%exp2( 2, 1 ).

exp2( Result, X ) :-
	( X #= 0, Result #= 1
	; ( X #= 1, Result #= 2
	  ; X #> 1,
	           ( X #=< 32, exp2XlessThan33( Result, X )
		   ; X #> 32, X1 #= X - 32, exp2XlessThan33( R, X1), Result #= 4294967296 * R
		   )
	  )
	).

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
%div2exp2( Result, X, A, N ) :-
%	A * C0 $= X,
%	N1 #= N + 1,
%	Err * exp( N1 * ln( 2.0 ) ) $= 1.0,
%% погрешность при делении на 2^N берем равной 1/2^(N+1)
%	C_real $= C0 + Err,
%	integers([Result]),
%	Result $=< C_real,
%	C_real $< Result + 1 .
	0 #=< D, D #< A,
	X #= Result * A + D. 



% getbit:  Result is getbit( Number, Index )
% самый младший бит - нулевой

getbit2( Result, Number, 0 ) :-
	( Number #= 2 * _, Result #= 0 ; Number #= 2 * _ + 1, Result #= 1 ).
getbit2( Result, Number, Index ) :-
	Index #> 0,
	( Number #= 2 * X ; Number #= 2 * X + 1 ),
	Index #= I1 + 1,
	getbit2( Result, X, I1 ).

% Result = младш.бит(Number / 2^Index)
getbit( Result, Number, Index ) :-
	integers([Result, Number, Index]),
	exp2( A, Index ),
%	div2exp2( C, Number, A, Index ),
	div2exp2( C, Number, A ),
	( C #= 2*_, Result #= 0 ; C#= 2*_ + 1, Result #= 1).
	
	
%lib(ic),I #>= 0, X #>= 10000000, operations:is_int(X), ic:(I * ln(2.0) =< ln(X)), indomain(X), indomain(I), operations:getbit( R1, X, I ), getbit( X, I, R ), write("X = "), writeln(X),R #\= R1

getbits3( Result, Number, 0, 0 ) :-
	getbit( Result, Number, 0 ) .
getbits3( Result, Number, EndIndex, 0 ) :-
	EndIndex #> 0,
	( Number #= 2 * X, Result #= 2 * R ; Number #= 2 * X + 1, Result #= 2 * R + 1 ),
	EndIndex #= E + 1,
	getbits3( R, X, E, 0 ).
getbits3( Result, Number, EndIndex, StartIndex ) :-
	StartIndex #> 0,
	( Number #= 2 * X ; Number #= 2 * X + 1 ),
	EndIndex #= E + 1,
	StartIndex #= S + 1,
	getbits3( Result, X, E, S ).
	

% getbits3( R3, X, E, S ), getbits( R, X, E, S ), ic:(R $\= R3)
getbits2( Result, Number, EndIndex, StartIndex ) :-
	Whole #= 2 ^ (EndIndex - StartIndex + 1),
	Result #= ( (Number >> StartIndex) mod Whole ) << StartIndex.

% A := 2^StartIndex
% C := Number/A  (Number >> StartIndex)
% D := 2^(EndIndex - StartIndex + 1)
% Result >= 0, Result < D, integers([Result]), C = _ * D + Result
getbits( Result, Number, EndIndex, StartIndex ) :-
	integers([Result, Number, EndIndex, StartIndex]),
	EndIndex #>= StartIndex,

	exp2( A, StartIndex ),
%	div2exp2( C, Number, A, StartIndex ),
	div2exp2( C, Number, A ),

% D := 2^(EndIndex - StartIndex + 1)
	Pow #= EndIndex - StartIndex + 1,
	exp2( D, Pow ),

	Result #>= 0, Result #< D,
	integers([Result]), 
	C #= _ * D + Result .
		

concat2( Result, X, 0 ) :- 
	Result #= X * 2.
concat2( Result, X, Y ) :-
%	BitLenY #= fix( ln(Y) / ln(2.0) + 0.5 ) + 1,
%	Result #= ( X << BitLenY ) + Y.

%	BitLenWithEpsilon $= ln(Y) / ln(2.0) + 0.5 + 1,
%	BitLenY $=< BitLenWithEpsilon,  BitLenWithEpsilon $< BitLenY + 1,
%	integers([BitLenY]),
%
%	ExpWithEpsilon $= exp(BitLenY * ln(2)) + 0.5, 
%	Exp $=< ExpWithEpsilon, ExpWithEpsilon $< Exp + 1,
%	integers([Exp]),

%	A $= ln(Y),

	ln(Y) $= B * ln(2.0),
	BitLenWithEpsilon $= B + 1.5,
	BitLenY $=< BitLenWithEpsilon,
	BitLenWithEpsilon $< BitLenY + 1,
	integers([BitLenY]),

	C $= BitLenY * ln(2),
	D $= exp( C ),
	ExpWithEpsilon $= C + 0.5, 
	Exp $=< ExpWithEpsilon,
	ExpWithEpsilon $< Exp + 1,
	integers([Exp]),
	
	integers([Result, X, Y]),

	Result #= ( X * Exp ) + Y.

power( X, 0, X ).
power( X, K, Result ) :-
	K1 is K - 1,
	power( X, K1, Result1 ),
	concat( X, Result1, Result ).
	

%is_int( X ) :- X #:: [ -2147483648 .. 2147483647 ] .
%is_long( X ) :- X #:: [ -9223372036854775808 .. 9223372036854775807 ] .
is_int( X ) :- X #:: [ 0 .. 2147483647 ] .
is_long( X ) :- X #:: [ 0 .. 9223372036854775807 ] .

bitlen4( 1, 0 ) .
bitlen4( Result, X ) :-
	X #> 0,
	X $= exp(A),
	A $= B * ln(2.0),
% погрешность при взятии двоичного логарифма от N равна (2N-1)/4N^2
	Err $= (2 * X - 1) / (4 * sqr( X ) ),
	C $= B + 1 + Err,
	Result $=< C,
	C $< Result + 1,
	integers([X]),
	integers([Result]).

bitlen( 1, 0 ).
bitlen( Result, X ) :-
	R #< 2 * X, X #=< R, exp2( R, Result ). 

% неправильно работает при X > 2^1025
bitlen2( 1, 1, 2 ).
bitlen2( Result, X, Power ) :-
	X #> 1,
	(X #= 2 * X1 ; X #= 2 * X1 + 1),
	Result #= Result1 + 1,
	Power #= Power1 * 2,
	bitlen2( Result1, X1, Power1 ).

%% неправильно работает при X = 9007199254740992, X = 9007199254740993
%bitlen3( 1, 0, 1 ).
%% 2^(bitlen-1) =< x < 2^bitlen
%bitlen3( Result, X, Power ) :-
%	bitlen3cycle( Result, X, Power, 1, 1 ) .
%bitlen3cycle( Result, X, Power, I, Pminus1 ) :-
%	( 
%		Pminus1 #=< X, X #< 2 * Pminus1, Result #= I, Power #= 2 * Pminus1 ; 
%		X #>= 2*Pminus1, I1 #= I + 1, Pminus12 #= 2 * Pminus1, bitlen3cycle( Result, X, Power, I1, Pminus12 )
%	).
	
concat( Result, X, 0 ) :-
	Result #= X * 2.
concat( Result, X, Y ) :-
	bitlen( BL, Y ),
	exp2( Power, BL ),
	Result #= ( X * Power ) + Y.

% Result = X.X.X.X...X (pow раз)
pow2( X, X, 1 ).
pow2( Result, X, Pow ) :-
	Pow #> 1,
	Pow #= Pow1 + 1, 
	pow( R, X, Pow1 ),
	concat( Result, R, X ).

pow( X, X, 1 ).
pow( Result, X, 2 ) :- 
	concat(Result, X, X).
pow( Result, X, N ) :-
	N #> 2,
	
	bitlen( BL, X ),
	
	N_BL #= N * BL,
	exp2( AN, N_BL ),

	D #= X * ( AN - 1 ),

	exp2( A, BL ),

	A1 #= A - 1,
%	div2exp2( Result, D, A1, BL ).
	div2exp2( Result, D, A1 ).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
random_result( X ) :-
	get_domain(X, L),
	( compound(L), 
	random_element(L, Fs1),
	X #>= Fs1,
	get_min(X, X)
	; integer(L) )
.

signed2unsigned( Xnew, X, SizeOfX ) :-
	X #< 0,
	exp2( A, SizeOfX),
	2*X #>= -A,
	Xnew #= X + A.