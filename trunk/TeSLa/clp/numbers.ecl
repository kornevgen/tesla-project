:- module( numbers ).
:- lib( ic ).
:- use_module( scalar ).

:- export sizeof/2.

:- export greaterUnsigned/3.
:- export lessUnsigned/3.
:- export greaterORequalUnsigned/3.
:- export lessORequalUnsigned/3.
:- export notequal/3.
:- export equal/3.

:- export getbit/4.
:- export getbits/5.
:- export concat/5.

:- export number2number/3.

:- export random_result/1.

chunksize( 51 ).  %parameter

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

is_chunk( X ) :- X #>= 0, chunksize( C ), exp2( D2, C ), X #< D2 .

sizeof( X, Size ) :-
	nonvar( Size ), Size > 0,
	chunksize( C ),
	H is Size mod C,
	( H = 0 -> Len is Size div C ; Len is Size div C + 1 ),
	length( X, Len ),
	checklist( is_chunk, X ),
	( H = 0 -> true ; X = [Xt|_], exp2(D, H), Xt #< D ) .
	
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

greaterUnsigned( X, Y, Size ) :-
	sizeof( X, Size ), sizeof( Y, Size ),
	
	X = [ Xh | Xt ], Y = [ Yh | Yt ],
	
	chunksize( C ),
	M is Size mod C,
	( M = 0 -> S is Size - C ; S is Size - M ),
	 
	( Xh #> Yh
	; Xh #= Yh, greaterUnsigned( Xt, Yt, S ) ) .
	
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

lessUnsigned( X, Y, Size ) :-
	sizeof( X, Size ), sizeof( Y, Size ),
	
	X = [ Xh | Xt ], Y = [ Yh | Yt ],
	
	chunksize( C ),
	M is Size mod C,
	( M = 0 -> S is Size - C ; S is Size - M ),
	 
	( Xh #< Yh
	; Xh #= Yh, lessUnsigned( Xt, Yt, S ) ) .
	
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

greaterORequalUnsigned( X, Y, Size ) :-
	sizeof( X, Size ), sizeof( Y, Size ),
	
	X = [ Xh | Xt ], Y = [ Yh | Yt ],
	
	chunksize( C ),
	M is Size mod C,
	( M = 0 -> S is Size - C ; S is Size - M ),
	 
	( Xh #> Yh
	; Xh #= Yh, greaterORequalUnsigned( Xt, Yt, S ) ) .
greaterORequalUnsigned( [], [], 0 ).
	
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

lessORequalUnsigned( X, Y, Size ) :-
	sizeof( X, Size ), sizeof( Y, Size ),
	
	X = [ Xh | Xt ], Y = [ Yh | Yt ],
	
	chunksize( C ),
	M is Size mod C,
	( M = 0 -> S is Size - C ; S is Size - M ),
	 
	( Xh #< Yh
	; Xh #= Yh, lessORequalUnsigned( Xt, Yt, S ) ) .
lessORequalUnsigned( [], [], 0 ).
	
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

notequal( X, Y, Size ) :-
	sizeof( X, Size ), sizeof( Y, Size ),

	X = [ Xh | Xt ], Y = [ Yh | Yt ],
	
	chunksize( C ),
	M is Size mod C,
	( M = 0 -> S is Size - C ; S is Size - M ),
	 
	( Xh #\= Yh
	; Xh #= Yh, notequal( Xt, Yt, S ) ) .
	
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

equal( X, Y, Size ) :-
	sizeof( X, Size ), sizeof( Y, Size ),

	X = [ Xh | Xt ], Y = [ Yh | Yt ],
	
	chunksize( C ),
	M is Size mod C,
	( M = 0 -> S is Size - C ; S is Size - M ),
	 
	Xh #= Yh, equal( Xt, Yt, S ) .
equal( [], [], 0 ).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

getbit( [Bit], X, SizeOfX, Index ) :-
	sizeof( X, SizeOfX ),

% looking for endeed chunck of X
	chunksize( C ),
	N is Index div C,
	length( X2, N ),
	append( _, [ Xh | X2 ], X ),

	I is Index mod C,	
	getbitFromNumber( Bit, Xh, I ) .
	
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% учесть случаи EndIndex и StartIndex на границах, EndIndex и StartIndex в одном chunk'е !!!
getbits( Bits, X, SizeOfX, EndIndex, StartIndex ) :-
	sizeof( X, SizeOfX ),
	
	chunksize( C ), C1 is C - 1, 
%looking for chunk from edge to StartIndex
	NStart is StartIndex div C,
	NEnd is EndIndex div C,
	length( X2Start, NStart ),
	append( _, [ XhStart | X2Start ], X ),
	IStart is StartIndex mod C,
	IEnd is EndIndex mod C,
	
	( NStart #= NEnd ->
		Bits = [ B ],
		getbitsFromNumber( B, XhStart, IEnd, IStart )
	; NStart #< NEnd,
		getbitsFromNumber( BitsStart, XhStart, C1, IStart ),
		length( X2End, NEnd ),
		append( _, [ XhEnd | X2End ], X ),
		getbitsFromNumber( BitsEnd, XhEnd, IEnd, 0 ),
		
		N is NEnd - NStart - 1,
		BitsLen is N + 2,
		length( X2, N ),
		append( X2, _ ,  X2End ),
		append( [ BitsEnd | X2 ], [ BitsStart ], X1 ),
		( IStart = 0 ->
			Bits = X1
		;
			CMIS is C - IStart, defragment( Bits, X1, CMIS, BitsLen )
		)
	).

% [ 00ABC| D | E | 000FG ] -> [ ABCD | EFG ] StartSize = length("FG")
% missing of 0s before '*' in the last element of a list with shifting to the right the rest of the list
defragment( [ A ], [ A ], _, _ ) .
defragment( Normalized, Unnormalized, StartSize, SizeOfNumber ) :-
	chunksize( C ),
	ExtraSize is C - StartSize,
	
	%prepare U1 - version of Unnormalized without young element and left-shifted 'before-young' element
	append( Untail, [ A, B ], Unnormalized ),
	exp2( DS, StartSize ),
	B #< DS,
	exp2( DES, ExtraSize ),
	Bits #>= 0, Bits #< DES,
	A #= Bs * DES + Bits,
	append( Untail, [ Bs ], U1 ),
	% and defragment U1 to N1
	SizeMC is SizeOfNumber - C,
	( SizeMC =< 0 -> N1 = U1 ; defragment( N1, U1, StartSize, SizeMC ) ),

	NEnd is Bits * DS + B,
	append( N1, [ NEnd ], Normalized ).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

concat( Z, X, SizeOfX, Y, SizeOfY ) :-
	sizeof( X, SizeOfX ),
	sizeof( Y, SizeOfY ),
	
	chunksize( C ),
	LastBitsCount is SizeOfY mod C,
	ExtraBitsCount is C - LastBitsCount,
	
	append( Xrevtail, [ Xrevh ], X ),
	
	exp2( DEBS, ExtraBitsCount ),
	B #>= 0, B #< DEBS,
	Xrevh #= A * DEBS + B,
	
	( ExtraBitsCount >= SizeOfX -> 
		X2 = []
	 ;
	 	append( Xrevtail, [ A ], X1 ),
		SizeMC is SizeOfX - ExtraBitsCount,
		( SizeMC =< 0 ->
			X2 = X1
		;
			defragment( X2, X1, LastBitsCount, SizeMC )
		)
	),
	
	Y = [ Yh | Ytail ],
	exp2( DLBC, LastBitsCount ),
	Y1 #= Yh + B * DLBC,
	append( X2, [ Y1 | Ytail ], Z ).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

pow( Y, X, SizeOfX, N ) :-
	sizeof( X, SizeOfX ),
	
	chunksize( C ),
	LastBitCount is SizeOfX mod C,
	
	X = [ Xh | Xt ],
	
	exp2( DLBC, LastBitCount ),
	Xh #= Xa * 2^LBC + Xb,
	
	

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

random_result( X ) :-
	checklist( rnd_result, X ).
	
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

number2number( X, X, _ ).