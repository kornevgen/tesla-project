
greaterUnsigned( number{ high: H1, low: L1 }, number{ high: H2, low: L2 } ) :-
	( H1 #> H2
	; H1 #= H2, L1 #> L2 ).

greaterSigned(
	number{ high: H1, low: L1 },
	number{ high: H2, low: L2 }
 ) :-
	integers([ H1, L1, H2, L2 ]),
	D31 = 2147483648,
	
	( H1 #< D31, 	% X1 >= 0
		( H2 #>= D31 % X2 < 0
			% X1 >= 0 > X2
		; H2 #< D31, % X2 >= 0
			greaterUnsigned( number( H1, L1 ), number( H2, L2 ) )
		)
	; H1 #>= D31,	% X1 < 0
		H2 #>= D31,	% X2 < 0
		H11 #= H1 - D31,
		H21 #= H2 - D31,
		greaterUnsigned( number( H21, L2 ), number( H11, L1 ) )
	).

greaterORequalUnsigned( number{ high: H1, low: L1 }, number{ high: H2, low: L2 } ) :-
	( H1 #= H2, L1 #= L2 
	; greaterUnsigned( number( H1, L1), number( H2, L2 ) ) ).

greaterORequalSigned( number{ high: H1, low: L1 }, number{ high: H2, low: L2 } ) :-
	( H1 #= H2, L1 #= L2 
	; greaterSigned( number( H1, L1), number( H2, L2 ) ) ).

lessSigned( N1, N2 ) :- greaterSigned( N2, N1 ). 
lessUnsigned( N1, N2 ) :- greaterUnsigned( N2, N1 ). 
lessORequalSigned( N1, N2 ) :- greaterORequalSigned( N2, N1 ).
lessORequalUnsigned( N1, N2 ) :- greaterORequalUnsigned( N2, N1 ).

notequal( number{ high: H1, low: L1 }, number{ high: H2, low: L2 } ) :-
	( H1 #\= H2
	; L1 #\= L2 ).

equal( number{ high: H1, low: L1 }, number{ high: H2, low: L2 } ) :-
	L1 #= L2, H1 #= H2.
	
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%this is unsigned version!!!!
sumUnsigned( 
	number{ high: H, low: L },
	number{ high: H1, low: L1 },
	number{ high: H2, low: L2 }   ) :-
	
	LL #= L1 + L2, HH #= H1 + H2, 
	D32 = 4294967296, D32M1 = 4294967295,
	( LL #< D32, L #= LL,
		( HH #< D32, H #= HH ; HH #>= D32, H #= HH - D32 )
	; LL #>= D32, L #= LL - D32,
		( HH #< D32M1, H #= HH ; HH #>= D32M1, H #= HH - D32M1 )
	).

%signed
sum(
	number{ high: H, low: L },
	number{ high: H1, low: L1 },
	number{ high: H2, low: L2 }   ) :-
	
	LL #= L1 + L2, HH #= H1 + H2, 
	D32 = 4294967296,
	( LL #< D32, L #= LL,
		H #= HH
	; LL #>= D32, L #= LL - D32,
		H #= HH + 1
	).	
	
subUnsigned( 
	number{ high: H, low: L },
	number{ high: H1, low: L1 },
	number{ high: H2, low: L2 }   ) :-
	
	D32 = 4294967296,
	( L1 #>= L2,
		L #= L1 - L2, X = 0
	; L1 #< L2,
		L #= D32 + L1 - L2, X = 1
	),
	
	S #= H1 - H2 - X,
	
	( S #>= 0,
		H #= S
	; S #< 0,
		H #= D32 + S
	).

% signed
sub(
	number{ high: H, low: L },
	number{ high: H1, low: L1 },
	number{ high: H2, low: L2 }   ) :-
	
	D32 = 4294967296,
	( L1 #>= L2,
		L #= L1 - L2, X = 0
	; L1 #< L2,
		L #= D32 + L1 - L2, X = 1
	),
	
	S #= H1 - H2 - X,
	
	( S #>= 0,
		H #= S
	; S #< 0,
		H #= D32 + S
	).
	


mul( 
	number{ high: H, low: L },
	number{ high: H1, low: L1 },
	number{ high: H2, low: L2 }   ) :-
	
	mulModD32( L1, L2, L, X ),
	mulModD32( H1, L2, P1, _ ),
	mulModD32( H2, L1, P2, _ ),
	
	D32 = 4294967296, 0 #=< H, H #< D32,
	P1 + P2 + X #= _ * D32 + H.


% L = (A * B) mod 2^32
% X = (A * B) div 2^32
% A, B isin [0, 2^32)
mulModD32( A, B, L, X ) :-
	D16 = 65536, D32 = 4294967296,
	LL1 #>= 0, LL2 #>= 0, LL2 #< D16,
	A #= LL1 * D16 + LL2,
	LM1 #>= 0, LM2 #>= 0, LM2 #< D16,
	B #= LM1 * D16 + LM2,
	
	0 #=< Z, Z #< D16,
	LL2 * LM1 + LL1 * LM2 #= Y * D16 + Z,
	
	0 #=< L, L #< D32,
	Z * D16 + LL2 * LM2 #= S * D32 + L,

	X #= LL1 * LM1 + Y + S.
	
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

getbit2( number{ high: 0, low: Result}, number{ high: H, low: L }, SizeOfNumber, Index ) :-
	Index #>= 0, Index #< SizeOfNumber,
	( Index #< 32,
		operations:getbitFromNumber( Result, L, Index )
	; Index #>= 32,
		I1 #= Index - 32,
		operations:getbitFromNumber( Result, H, I1 )
	).

getbit( number{ high: 0, low: Result}, number{ high: H, low: L }, Index ) :-
	Index #>= 0,
	( Index #< 32,
		operations:getbitFromNumber( Result, L, Index )
	; Index #>= 32,
		I1 #= Index - 32,
		operations:getbitFromNumber( Result, H, I1 )
	).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

getbits( number{ high: ResultH, low: ResultL}, SizeOfResult, 
		 number{ high: H, low: L }, EndIndex, StartIndex ) :-

	StartIndex #>= 0, EndIndex #>= StartIndex,
	SizeOfResult #= EndIndex - StartIndex + 1,
	
	( EndIndex #< 32,
		operations:getbitsFromNumber( ResultL, SizeOfResult, L, EndIndex, StartIndex ),
		ResultH = 0
	; EndIndex #>= 32,
		E #= EndIndex - 32,
		( StartIndex #>= 32,
			S #= StartIndex - 32,
			operations:getbitsFromNumber( ResultL, SizeOfResult, H, E, S ),
			ResultH = 0
		; StartIndex #< 32,
			operations:getbitsFromNumber( X, _, L, 31, StartIndex ),
			S #= 32 - StartIndex,
			operations:exp2( DE, S ),
			( EndIndex #>= StartIndex + 32,
				E1 #= EndIndex - (StartIndex + 32),
				operations:getbitsFromNumber( Y, _, H, E1, 0 ),
				ResultL #= Y * DE + X,
				E2 #= E1 + 1,
				operations:getbitsFromNumber( ResultH, _, H, E, E2 )
			; EndIndex #< StartIndex + 32,
				operations:getbitsFromNumber( Y, _, H, E, 0 ),
				ResultL #= Y * DE + X,
				ResultH = 0
			)
		)
	).
	
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

concat( number{ high: H, low: L }, SizeOfResult,
		number{ high: H1, low: L1 }, Size1,
		number{ high: H2, low: L2 }, Size2
	) :-
		integers([H, L, H1, L1, H2, L2, SizeOfResult, Size1, Size2]),
		Size1 #>= 0, Size2 #>= 0,
		SizeOfResult #= Size1 + Size2,
		SizeOfResult #=< 70,
		H #>= 0, L #>= 0, H1 #>= 0, L1 #>= 0, H2 #>= 0, L2 #>= 0,
		
		( Size2 #< 32,
			H2 = 0,
			Size1M1 #= Size1 - 1,
			( SizeOfResult #< 32,
				H1 = 0,
				H = 0,
				operations:exp2( DEB, Size2 ),
				L #= L1 * DEB + L2
			; SizeOfResult #>= 32,
				ExtraSizeM1 #= 31 - Size2,
				ExtraSize #= ExtraSizeM1 + 1,
				( ExtraSize #>= Size1,
					L #= L2
				; ExtraSize #< Size1,
					operations:getbitsFromNumber( ExtraBits, ExtraSize, L1, ExtraSizeM1, 0 ),
					operations:exp2( DEB, Size2 ),
					L #= ExtraBits *  DEB + L2
				),
				getbits( number( 0, H ), _, number{ high: H1, low: L1 }, Size1M1, ExtraSize )
			)
		; Size2 #>= 32, %%=> Size1 < 32 => H1 = 0
			H1 = 0, H2 #> 0, 
			L #= L2,
			S #= Size2 - 32,
			exp2( DS, S ),
			H #= L1 * DS + H2
		).
		
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

power( number{ high: H, low: L}, SizeOfResult,
		number{ high: H1, low: L1}, Size1,
		Power
	) :-
		integers( [H, L, SizeOfResult, H1, L1, Size1, Power] ),
		Size1 #> 0, Power #>= 1,
		SizeOfResult #= Size1 * Power,
		SizeOfResult #=< 64,
		
		( Power = 1,
			H #= H1, L #= L1
		; Power = 2,
			H1 = 0,
			concat( number{ high: H, low: L }, SizeOfResult,
						number{ high: H1, low: L1 }, Size1,
						number{ high: H1, low: L1 }, Size1
			)
		; Power #>= 3, %%=> Size1 =< [64/3] <=>  Size1 =< 21
			H1 = 0,
			%% часть их влезет в L
			N1 #> 0,
			0 #=< ExtraSize0, ExtraSize0 #< Size1,
			32 #= N1 * Size1 + ExtraSize0, %% N1 штук  влезет полностью в L
			( N1 #> Power,
				N2 #= Power, ExtraSize = 0
			; N1 #=< Power,
				N2 #= N1, ExtraSize #= ExtraSize0
			),
			N #= N2 * Size1, operations:exp2( DN, N ),
			operations:exp2( DS, Size1 ),
			LL * (DS - 1) #= L1 * (DN - 1),
			( ExtraSize = 0,
				L #= LL
			; ExtraSize #>= 1,
				E #= ExtraSize - 1,
				operations:getbitsFromNumber( ExtraBits, ExtraSize, L1, E, 0 ),
				L #= LL + ExtraBits * DN
			),
			
			%% остальные уйдут в H
			P #= Power - N2 - 1,
			( P #=< 0,
				H = 0
			; P #>= 1,
				NN #= P * Size1, operations:exp2( DNN, NN ),
				LNN * (DS - 1) #= L1 * (DNN - 1),
				S1 #= Size1 - 1,
				operations:getbitsFromNumber( EB, SEB, L1, S1, ExtraSize ),
				operations:exp2( DSEB, SEB ),
				H #= LNN * DSEB + EB
			)
		).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
number2number( N, number{ high: H, low: L } ) :-
	integers( [N, H, L] ),
	( nonvar(N), H is N / 4294967296, L is N mod 4294967296
	; var(N),
		( var(H)
		; var(L)
		; nonvar(H), nonvar(L), N is H * 4294967296 + L
		)
	).
	
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
random_result( number{ high: H, low: L } ) :-
	operations:random_result( H ),
	operations:random_result( L ).