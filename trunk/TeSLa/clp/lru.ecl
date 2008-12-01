:- module( lru ).

:- lib( ic ).
:- lib( ic_sets ).

:- export setConditions/3.
:- export latestSetVar/5.
:- export initialize/5.
:- export addHit/4.
:- export latestNHits/3.
:- export addVytesnTag/4.
:- export addSetVar/3.
:- export vytesnTagsLRU/2.

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
	
	
%% [ 1, 2 | _ ] -> latestSetVar = 2
%latestSetVar( SetVar, LevelSets, SetNumber! ) :-
%	LevelSets = [ S | T ],
%	( free( T ), !, SetVar = S
%	; latestSetVar( SetVar, T )
%	) .
%	
%% [ 1, 2 | _ ] >< 3 -> [ 1, 2, 3 | _ ]
%upgradeSet( LevelSets, SetNumber!, NewSet ) :-
%	LevelSets = [ _ | T ],
%	( free( T ), !, T = [ NewSet | _ ]
%	; upgradeSet( T, NewSet )
%	) .
%	
%addHit( Tag, SetVar, LevelHits, SetNumber ) :-
%
%addVytesnTag( VytesnTag, LevelHits, SetNumber ) :-
%	% store VytesnTag at the end of LevelHits .
%	
%% ; Case_5 = 1, _5 = Level1Set0IniTag2, Level1Set0_0 \ _5set sameset []  \/ Level1Set0IniTag1set \/ _0set
%% ; Case_5 = 0, _5 = Level1Set0IniTag3, Level1Set0_0 \ _5set sameset []  \/ Level1Set0IniTag2set \/ Level1Set0IniTag1set \/ _0set
%% program: hit X1, hit X2, hit X3, hit X4, hit X5, miss X6 -> X65, hit X7
%% SetSize = 3, SetVar = S0
%% Hits = [ X1, X2, X3, X4, X5, X6, X7 ]
%% HitSets = [ X1set, X2set, X3set, X4set, X5set, X6set, X7set ]
%% VTag = X65, VTagSet = X65set, VTagIdx = 5 - индекс хита перед вытеснением
%% Case = 3 : X65 = X3, S0 \ X3set sameset X4set \/ X5set 
%% Case = 2 : X65 = X2, S0 \ X2set sameset X3set \/ X4set \/ X5set
%% Case = 1 : X65 = X1, S0 \ X1set sameset X2set \/ X3set \/ X4set \/ X5set
%%
%lru( Case, VTag, VTagSet, VTagIdx, SetVar, Hits, HitSets, LastHitIdx, SetSize ) :-
%	SetSize > 0,
%	LastHitIdx > 0, % TODO: более оптимально LastHitIdx > max{ 0, индекс вытеснения с номером, меньшим данного на SetSize - 1 }
%	( Case = LastHitIdx,
%		nth( LastHitTag, Hits, LastHitIdx ), VTag = LastHitTag,
%		% sets equality
%		(
%			%  HitTags: HT[LastHitIdx+1], HT[LastHitIdx+2], ..., HT[VTagIdx - SetSize]
%			L is LastHitIdx + 1,
%			for( I, L, VTagIdx )
%			fromto( [], In, Out, Diz )
%			do
%				nth_set( HTS, HitSets, I ),
%				Out = In \/ HTS
%		), 
%		SetVar \ VTagSet sameset Diz
%	; L1 is LastHitIdx - 1,
%		lru( Case, VTag, VTagSet, VTagIdx, SetVar, Hits, HitSets, L1, SetSize )
%	) .
%
%vytesnTagsLRU( ... ) :-
%	getHits( Hits, Level1, SetNum0L1 ),
%	getHitSets( HitSets, Level1, SetNum0L1 ),
%	reversedVytesnTag( _121, Level1, SetNum0L1, ... ), % but not more than N-1...
%	reversedVytesnTagIdxs( _1212, Level1, SetNum0L1, ... ), % but not more than N-1...
%	length( Hits, LH ), MaxCase is LH + 1,
%	( foreach( VT, _121 ),  % foreach vytesnTag :
%  	  foreach( VTIdx, _1212 ),  % foreach vytesnTag :
%  	  fromto( MaxCase, Case_Tagbefore, Case_Tagafter, _ )
%  	  param( Hits, HitSets )
%	do
%  	  Case_Tagafter < Case_Tagbefore,
%  	  getSetVarForIdx( S0, Level1, SetNum0L1, VTIdx ),
%  	  MaxLastHit is VTIdx - <<SetSizeOfLevel1-1>>,
%  	  lru( Case_Tagafter, VT, VTSet, VTIdx, S0, Hits, HitSets, MaxLastHit, <<SetSizeOfLevel1>> )
%	) .

% TODO нужны ли setvars ??  всё равно setvar есть у каждого в hit и vytesns
:- local struct( set( number, setvars, hits, vytesns ) ) .
:- local struct( hit( tag, tagset, hitset ) ) .
:- local struct( vytesn( tag, setvarbefore, index ) ) .

% если данный сет не проинициализирован, проинициализировать данным множеством, иначе отождествить его со значением сета
% SetNumber :: 0
% SetVar :: S0
% SetsStructure :: [ set( number: 1, setvars: [ S11, S12 | _ ], hits: [ hit( tag: R1, hitset: S11 ) | _ ] ) | _ ]
% Tags :: [ T1, T2, T3 ]
initialize( SetNumber, SetVar, SetsStructure, Tags, TagSets ) :-
	integer( SetNumber ),
% if SetNumber is known in SetsStructure then associate Tags, otherwise add SetNumber +> Tags
	( free( SetsStructure ) 
		-> reverse( Tags, RTags ), reverse( TagSets, RTagSets ),
			( foreach( T, RTags ), foreach( TS, RTagSets ), fromto( _, In, Out, HTags ), param( SetVar ) 
			 do
			 Out = [ hit{ tag:T, tagset: TS, hitset: SetVar } | In ]
			),
		  SetsStructure = [ set{
				  number: SetNumber
				, setvars: [ SetVar | _ ]
				, hits: HTags
			 } | _ ]
		; SetsStructure = [ set{ number: SN %, hits: Hits
		 } | L ],
			( SN = SetNumber -> 
				true %reverse( Tags, RTags ),
				%( foreach( H, Hits ), foreach( T, RTags )
				%do
				%	H = T
				%)
			; initialize( SetNumber, SetVar, L, Tags, TagSets )
			)
	) .
	
% [ 1, 2 | _ ] -> latest = 2
latest( S, [ S1 | L ] ) :-
	( free( L ), !, S = S1
	; latest( S, L )
	) .


% S :: S12 (result)
% Hs :: [ hit{ tag: R1, hitset: S11 } | _ ] (result)
% VTs :: [ vytesn{ tag: R2, setvarbefore: S11 } | _ ] (result)
% SetsStructure :: [ set{ number: 1, setvars: [ S11, S12 | _ ], hits: [ hit{ tag: R1, hitset: S11 } | _ ], vytesns:[vytesn{...}|_] } | _ ]
% SetNumber :: 0
latestSetVar( S, Hs, VTs, SetsStructure, SetNumber ) :-
	integer( SetNumber ),
	SetsStructure = [ set{ number: SN, setvars: Sets, hits: Hits, vytesns: VTags } | L ],
	( free( L ), !, SN = SetNumber, latest( S, Sets ), Hs = Hits, VTs = VTags
	; latestSetVar( S, Hs, VTs, L, SetNumber )
	) .
	
	
% Tag :: R1
% LatestSetVar :: S11
% HitsStructure :: [ hit{ tag: R1, hitset: S11 } | _ ]	(result)
addHit( Tag, TagSet, LatestSetVar, HitsStructure ) :-
	( free( HitsStructure )
		->	HitsStructure = [ hit{ tag: Tag, tagset: TagSet, hitset: LatestSetVar } | _ ]
		
		;	HitsStructure = [ _ | L ],
			addHit( Tag, TagSet, LatestSetVar, L )
	) .
	
	
% NHits :: [ R1 ] (result)
% N :: 1
% HitsStructure :: [ hit{ tag: R1, hitset: S11 } | _ ]	(result)
latestNHits( NHits, N, HitsStructure ) :-
	getHitsOnly( Hits, HitsStructure ),
	length( NH, N ),
	append( _, NH, Hits ),
	( foreach( H, NH ),
	  fromto( [], In, [ T | In ], NHits )
	do
		H = hit{ tag: T }
	) .

getHitsOnly( H, Hs ) :-
	free( Hs ) -> H = [] ; Hs = [ H1 | L ], H = [ H1 | H2 ], getHitsOnly( H2, L ) .



% VTag :: X
% LatestSetVar :: S
% VytesnStructure :: [ vytesn{..}, vytesn{..} | _ ] (result) add vytesn(X,S) to this
addVytesnTag( VTag, LatestSetVar, HitsStructure, VytesnStructure ) :-
	( free( VytesnStructure )
		->	count( C, HitsStructure ),
			VytesnStructure = [ vytesn{ tag: VTag, setvarbefore: LatestSetVar, index: C } | _ ]		
		;	VytesnStructure = [ _ | L ],
			addVytesnTag( VTag, LatestSetVar, HitsStructure, L )
	) .
count( C, HitsStructure ) :-
	free( HitsStructure ) 
		->	C = 0
		;	HitsStructure = [ _ | L ],
			count( C1, L ),
			C is C1 + 1 .



% SetVar :: S
% SetNumber :: 0
% SetsStructure :: [ set{ number:0, setvars:[], ... } | _ ]
addSetVar( SetVar, SetsStructure, SetNumber ) :-
	nonvar( SetsStructure ),
	SetsStructure = [ set{ number: N, setvars: S } | L ],
	( N = SetNumber -> addToTheEnd( SetVar, S )
	; addSetVar( SetVar, L, SetNumber )
	) .
addToTheEnd( SetVar, Sets ) :-
	free( Sets ) -> Sets = [ SetVar | _ ] ; Sets = [ _ | L ], addToTheEnd( SetVar, L ) .



% преобразует все wildcard-хвосты в пустые множества
closeSets( SetsStructure ) :-
	closeList( SetsStructure ),
	( foreach( S, SetsStructure )
	do
		S = set{ setvars: Vars, hits: Hs, vytesns: VTs },
		closeList( Vars ),
		closeList( Hs ),
		closeList( VTs )
	) .
closeList( L ) :-
	free( L ) -> L = [] ; L = [ _ | L1 ], closeList( L1 ).		
		



% ; Case_5 = 1, _5 = Level1Set0IniTag2, Level1Set0_0 \ _5set sameset []  \/ Level1Set0IniTag1set \/ _0set
% ; Case_5 = 0, _5 = Level1Set0IniTag3, Level1Set0_0 \ _5set sameset []  \/ Level1Set0IniTag2set \/ Level1Set0IniTag1set \/ _0set
% program: hit X1, hit X2, hit X3, hit X4, hit X5, miss X6 -> X65, hit X7
% SetSize = 3, SetVar = S0
% Hits = [ X1, X2, X3, X4, X5, X6, X7 ]
% HitSets = [ X1set, X2set, X3set, X4set, X5set, X6set, X7set ]
% VTag = X65, VTagSet = X65set, VTagIdx = 5 - индекс хита перед вытеснением
% Case = 3 : X65 = X3, S0 \ X3set sameset X4set \/ X5set 
% Case = 2 : X65 = X2, S0 \ X2set sameset X3set \/ X4set \/ X5set
% Case = 1 : X65 = X1, S0 \ X1set sameset X2set \/ X3set \/ X4set \/ X5set
%
lru( Case, VT, Hits, RangeEnd, RangeStart ) :-
	RangeEnd >= RangeStart,
	(	Case = RangeEnd,
		VT = vytesn{ tag: VTag, setvarbefore: S, index: Index },
		nth( hit{ tag: HTag, tagset: HTagSet }, Hits, RangeEnd ),
		R is RangeEnd + 1,
		range( Hs, Hits, Index, R ),
		VTag #= HTag,
		(	foreach( H, Hs ),
			fromto( [], In, Out, D )
		do
			H = hit{ tagset: HitSet },
			Out = In \/ HitSet
		),
		S \ HTagSet sameset D
	; R is RangeEnd - 1, lru( Case, VT, Hits, R, RangeStart )
	) .
nth( E, List, Index ) :-
	length( X, Index ),
	append( X, [ E | _ ], List ) .
range( Es, List, End, Start ) :-
	length( X, Start ),
	EL is End - Start,
	length( Es, EL ),
	append( X, Y, List ),
	append( Es, _, Y ) .

% формулирует доп.предикаты на SetsStructure, описывающие lru вытесняемых тегов
vytesnTagsLRU( SetsStructure, SetSize ) :-
	closeSets( SetsStructure ),
	( foreach( Set, SetsStructure ),
	  param( SetSize )
	do
		Set = set{ hits: Hits, vytesns: VTs },
		reverse( VTs, RVTs ),
		length( Hits, LH ), MaxCase is LH + 1,
		( fromto( RVTs, [ VT | Out ], Out, [] ),
		  fromto( MaxCase, Casebefore, Caseafter, _ ),
		  param( SetSize, Hits )
		do
			VT = vytesn{ index: Index },
			CandidatesRangeEnd is Index - SetSize, % TODO +1 ?
			length( Out, OutLen ),
			( OutLen < SetSize 
				->	CandidatesRangeStart is 0
				;	length( RS, SetSize ), append( RS, [ vytesn{ index: C } | _ ], Out ),
					CandidatesRangeStart is C + 1
			),
			Caseafter #< Casebefore,
			lru( Caseafter, VT, Hits, CandidatesRangeEnd, CandidatesRangeStart )
		)
	) .
