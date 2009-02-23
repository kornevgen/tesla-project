:- module( tlb ).
:- lib(ic).
:- use_module( numbers ).

:- export key/11.
:- export addRow/2.
:- export closeRows/1.
:- export onlyOne/1.
:- export labelRows/1.
:- export refill/7.

:- export struct( tlbtag( index, range, vpnd2, mask, g, asid ) ).
:- export oddbit/7.

:- export addPfn/12.
:- export labelTLB/2.

key( VirtualAddress, SizeOfVA, Index, G, Asid,
	tlbtag{
		index:Index,
		range:Range,
		mask:Mask,
		vpnd2:VPNd2,
		g:G,
		asid:Asid
	  },
	RangeEndBit, RangeStartBit, MaxMask,
	VPNd2EndBit, VPNd2StartBit ) :-
	range( Range, VirtualAddress, SizeOfVA, RangeEndBit, RangeStartBit ),
	Mask #:: [ 0 .. MaxMask ],
	vp( VPNd2, VirtualAddress, SizeOfVA, VPNd2EndBit, VPNd2StartBit, Mask ).
	
% r := va[rend..rstart]
range( Range, VirtualAddress, SizeOfVA, RangeEndBit, RangeStartBit ) :-
	numbers:getbits( Range, VirtualAddress, SizeOfVA, RangeEndBit, RangeStartBit ).
	
% vp := va[vpend..vpstart+1+mask]
vp( [VPNd2], VirtualAddress, SizeOfVA, VPNd2EndBit, VPNd2StartBit, Mask ) :-
% более общая и правильная реализация:
%	Start #= VPNd2StartBit + 1 + Mask,
%	numbers:getbits2( VPNd2, VirtualAddress, VPNd2EndBit, Start ).
% ограниченная реализация: VPN2 должен влезать в один chunk
	Start is VPNd2StartBit + 1,
	numbers:getbits( [X], VirtualAddress, SizeOfVA, VPNd2EndBit, Start ),
	VPNd2 #>= 0,
	scalar:exp2( DM, Mask ),
	U #>= 0, U #< DM,
	X #= VPNd2 * DM + U .

% oddbit := va[vpstart+mask]
oddbit( Odd, Index, VirtualAddress, SizeOfVA, VPNd2StartBit, MaxMask, Rows ) :-
% ограниченная реализация
	EndBit is VPNd2StartBit + MaxMask,
	numbers:getbits( [X], VirtualAddress, SizeOfVA, EndBit, VPNd2StartBit ),
	% Odd is one bit from X ...
	getMask( Mask, Rows, Index ),
	Mask #>= 0, Mask #=< MaxMask,
	Odd #:: [0..1],
	( Mask = 0 -> 
		UU #>= 0,
		X #= UU * 2 + Odd
	;
		(Mask = 1 -> DM = 1; scalar:exp2( DM, Mask ) ),
		U #>= 0,  U #< DM,
		UU #>= 0,
		X #= UU * 2 * DM + Odd * DM + U
	) .

	
addRow( Rows, TLBtag ) :-
	( free( Rows )
		-> Rows = [ TLBtag | _ ]
		; Rows = [ tlbtag{index:I, range:Rg, mask:M, vpnd2:V} | Tl ],
			TLBtag = tlbtag{index:Index, range:Range, mask:Mask, vpnd2:VPNd2 },
			( I = Index
				-> Range #= Rg, Mask #= M, VPNd2 #= V
				; addRow( Tl, TLBtag )
			)
	) .

closeRows( Rows ) :-
	( free( Rows )
		-> Rows = []
		; Rows = [ _ | R ], closeRows( R )
	).
	
onlyOne( [] ) .
onlyOne( [ _ ] ) .
onlyOne( [ TLBtag | Tl ] ) :-
	( foreach( X, Tl ),
	  param( TLBtag )
	do
		TLBtag = tlbtag{range:R1, mask:M1, vpnd2:V1},
		X = tlbtag{range:R2, mask:M2, vpnd2:V2},
		( R1 #\= R2
		; vp2( VV, M2, V1 ), VV #\= V2
		; vp2( VV, M1, V2 ), VV #\= V1
		)
	),
	onlyOne( Tl ).
	
% V := VPNd2[end..Mask+1]
vp2( V, Mask, VPNd2 ) :-
	V #>= 0,
	scalar:exp2( DM, Mask ),
	U #>= 0, U #< 2*DM,
	VPNd2 #= V * 2 * DM + U .


refill( Rows, 
		VirtualAddress, SizeOfVA, 
		RangeEndBit, RangeStartBit,
		VPNd2EndBit, VPNd2StartBit
 ) :-
	( foreach( X, Rows ),
	  param( VirtualAddress ),
	  param( SizeOfVA ),
	  param( RangeEndBit ),
	  param( RangeStartBit ),
	  param( VPNd2EndBit ),
	  param( VPNd2StartBit )
	do
		X = tlbtag{range:R, mask:M, vpnd2:V},
		( 
			range( Range, VirtualAddress, SizeOfVA, RangeEndBit, RangeStartBit ),
			RSize is RangeEndBit - RangeStartBit + 1,
			numbers:notequal( Range, R, RSize )
		; 
			vp( [V1], VirtualAddress, SizeOfVA, VPNd2EndBit, VPNd2StartBit, M ),
			vp2( V2, M, V ),
			V1 #\= V2
		)
	).	 
	
labelRows( Rows ) :-
	( foreach( X, Rows )
	do
		X = tlbtag{range:R, mask:M, vpnd2:V, g:G, asid:Asid},
		numbers:random_result( R ),
		indomain( M ),
		numbers:random_result( V ),
		indomain( G, max ),
		numbers:random_result( Asid )
	) .		
	
getMask( Mask, [ tlbtag{mask:Mask,index:Index}|_], Index ) :- !.
getMask( Mask, [ _ |L ], Index ) :- getMask( Mask, L, Index ).

addPfn( 
	TLB, 
	Rows, Index,
	VirtualAddress, SizeOfVA, VPNd2StartBit, 
	PhysAddress, SizeOfPA, PFNEndBit, PFNStartBit,
	Valid, Modify
 ) :-
	oddbit2( Odd, Index, VirtualAddress,  SizeOfVA, VPNd2StartBit, Rows ),
	numbers:getbits( PFN, PhysAddress, SizeOfPA, PFNEndBit, PFNStartBit ),
	addPfnToTLB( TLB, PFN, Rows, Index, Odd, Valid, Modify ).

:- local struct( tlbrow( index, tlbtag, pfn0, v0, d0, pfn1, v1, d1 ) ).

addPfnToTLB( TLB, PFN, [ TLBtag|_], Index, Odd, Valid, Modify ) :-
	TLBtag = tlbtag{ index:Index }, !,
	( Odd = 0 -> addToTLB( TLB, tlbrow{ index:Index, tlbtag:TLBtag, pfn0: PFN, v0: Valid, d0: Modify } )
	; addToTLB( TLB, tlbrow{ index:Index, tlbtag:TLBtag, pfn1: PFN, v1: Valid, d1: Modify } )
	).
addToTLB( TLB, Row ) :-
	( free( TLB ) -> TLB = [ Row | _ ]
	; TLB = [ R | T ], R = tlbrow{ index: I },
		Row = tlbrow{ index: Index },
		( I = Index, R = Row ; I \= Index, addToTLB( T, Row ) )
	).

% oddbit := va[vpstart+mask]
oddbit2( Odd, Index, VirtualAddress, SizeOfVA, VPNd2StartBit, Rows ) :-
	getMask( Mask, Rows, Index ),
	Start is Mask + VPNd2StartBit,
	numbers:getbit( [Odd], VirtualAddress, SizeOfVA, Start ).

labelTLB( TLB, SizeOfPFN ) :-
	( foreach( T, TLB )
	, param( SizeOfPFN )
	do
		T = tlbrow{ pfn0: PFN0, v0:V0, d0:D0, pfn1: PFN1, v1:V1, d1:D1 },
		numbers:sizeof( PFN0, SizeOfPFN ),
		numbers:sizeof( PFN1, SizeOfPFN ),
		numbers:random_result( PFN0 ),
		numbers:random_result( PFN1 ),
		( free(V0) -> V0 = 1 ;true ),
		( free(V1) -> V1 = 1 ;true ),
		( free(D0) -> D0 = 1 ;true ),
		( free(D1) -> D1 = 1 ;true )
	).