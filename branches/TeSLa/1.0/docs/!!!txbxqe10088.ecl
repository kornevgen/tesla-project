:- module( txbxqe10088 ).
:- lib( ic ).
:- lib( ic_sets ).
:- use_module( lru ).
:- use_module( numbers ).
:- use_module( predicates ).

:- export go/7.

go( _, Level1Set0, Level2Set0, _0x, _0y, _0z, _0c ) :-
Level1Set0 = [ Level1Set0IniTag1, Level1Set0IniTag2, Level1Set0IniTag3 ],
Level1Set0 #:: [ 0 .. 15 ],
ic_global:alldifferent( Level1Set0 ),
intset( Level1Set0IniTag1set, 0, 15 ), #( Level1Set0IniTag1set, 1 ), Level1Set0IniTag1 in Level1Set0IniTag1set,
>> addHit( 
intset( Level1Set0IniTag2set, 0, 15 ), #( Level1Set0IniTag2set, 1 ), Level1Set0IniTag2 in Level1Set0IniTag2set,
>> addHit(
intset( Level1Set0IniTag3set, 0, 15 ), #( Level1Set0IniTag3set, 1 ), Level1Set0IniTag3 in Level1Set0IniTag3set,
>> addHit(

Level1Set0_0 = [] \/ Level1Set0IniTag1set \/ Level1Set0IniTag2set \/ Level1Set0IniTag3set,
>> Level1 = [ Level1Set0_0 | _ ],

Level2Set0 = [ Level2Set0IniTag1, Level2Set0IniTag2, Level2Set0IniTag3, Level2Set0IniTag4, Level2Set0IniTag5 ],
Level2Set0 #:: [ 0 .. 63 ],
ic_global:alldifferent( Level2Set0 ),
intset( Level2Set0IniTag1set, 0, 63 ), #( Level2Set0IniTag1set, 1 ), Level2Set0IniTag1 in Level2Set0IniTag1set,
intset( Level2Set0IniTag2set, 0, 63 ), #( Level2Set0IniTag2set, 1 ), Level2Set0IniTag2 in Level2Set0IniTag2set,
intset( Level2Set0IniTag3set, 0, 63 ), #( Level2Set0IniTag3set, 1 ), Level2Set0IniTag3 in Level2Set0IniTag3set,
intset( Level2Set0IniTag4set, 0, 63 ), #( Level2Set0IniTag4set, 1 ), Level2Set0IniTag4 in Level2Set0IniTag4set,
intset( Level2Set0IniTag5set, 0, 63 ), #( Level2Set0IniTag5set, 1 ), Level2Set0IniTag5 in Level2Set0IniTag5set,

Level2Set0_0 = [] \/ Level2Set0IniTag1set \/ Level2Set0IniTag2set \/ Level2Set0IniTag3set \/ Level2Set0IniTag4set \/ Level2Set0IniTag5set,
>> Level2 = [ Level2Set0_0 | _ ],



'ADD@overflow#0::main'( _, _0x, _0y, _0z, _1x, _0y, _0z),
'LW@noexception#1::main'( _, _0z, _1x, _0c, _1z, _1x, _0c, _0, _1),
>> SetNum0L1 -- эта переменная перед этим инициализируется (в задаче получения сетов)
>> SetNum0L2

%% hit _0 in set0 of Level1 which has version 0 

>> lru:currentSetVar( _111, Level1, SetNum0L1 )
>! _0 in _111, %% Level2Set0_0,
intset( _0set, 0, 15 ), #( _0set, 1 ), _0 in _0set,
>> addHit( _0, _0set, Level1Hits, SetNum0L1 ),



%% miss _1 -> _2 in set0 of Level2 which has version 0

% вытесняемый тег
>> lru:latestSetVar( _112, Level2, SetNum0L2 ),
>! _2 in _112, %%Level2Set0_0,
intset( _2set, 0, 63 ), #( _2set, 1 ), _2 in _2set,
>> addVytesnTag( _2, _333, Level2Hits, SetNum0L2 ), 

% тег - причина промаха
_1 #:: [ 0 .. 63 ],
>! _1 notin _112, %%Level2Set0_0,
intset( _1set, 0, 63 ), #( _1set, 1 ), _1 in _1set,

>! _113 = (( _112 \ _2set ) \/ _1set), %%Level2Set0_1 = (( Level2Set0_0 \ _2set ) \/ _1set),
>> lru:upgradeSet( Level2, SetNum0L2, _113 ),
>> lru:addHit( Level2, SetNum0L2, _1)

'ADD@overflow#2::main'( _, _1z, _0y, _1x, _2z, _0y, _1x),
'LW@noexception#3::main'( _, _0y, _2z, _0c, _1y, _2z, _0c, _3, _4), % Set1L1, Set1L2
% вытесняемый тег
_5 in Level1Set0_0,
intset( _5set, 0, 15 ), #( _5set, 1 ), _5 in _5set,

% тег - причина промаха
_3 #:: [ 0 .. 15 ], _3 notin Level1Set0_0,
intset( _3set, 0, 15 ), #( _3set, 1 ), _3 in _3set,

Level1Set0_1 = (( Level1Set0_0 \ _5set ) \/ _3set),

_4 in Level2Set0_1,
intset( _4set, 0, 63 ), #( _4set, 1 ), _4 in _4set,


какие нужны структуры?

для каждой команды --
* номера сетов для каждого уровня S

для каждого уровня -- для каждого сета --
* HitTags = [ X1, X2, ... ]
* HitSets = [ X1set, X2set, ... ]
* CurrentSets = [ S1, S1, ... ] % очередной элемент меняется после каждого miss'a
* VytesnTags = [ X65, ... ]
* VytesnTagSets = [ X65set, ... ]
* VytesnTagIdx = [ 5, ... ] % индекс хита перед вытеснением

уровень известен на этапе компляции, а номер сета нет => структуры будут такими:
HitTagsStructureOfLevel1 = [ hitTags( set = 10, hitTags = [...] ), ...  ]

% LRU predicates
>> % Level1: SetNum0L1, ... - здесь должны быть только разные сеты!!!

>> % Level1-SetNum0L1:  -- сет зафиксирован; можно работать с тегами в нем
>> lru:getHits( Hits, Level1, SetNum0L1 ),
>> lru:getHitSets( HitSets, Level1, SetNum0L1 ),
>> reversedVytesnTag( _121, Level1, SetNum0L1, ... ), % but not more than N-1...
>> reversedVytesnTagIdxs( _1212, Level1, SetNum0L1, ... ), % but not more than N-1...
>> length( Hits, LH ), MaxCase is LH + 1,
>> ( foreach( VT, _121 ),  % foreach vytesnTag :
>>   foreach( VTIdx, _1212 ),  % foreach vytesnTag :
>>   fromto( MaxCase, Case_Tagbefore, Case_Tagafter, _ )
>>   param( Hits, HitSets )
>> do
>>  Case_Tagafter < Case_Tagbefore,
>> 	lru:getSetVarForIdx( S0, Level1, SetNum0L1, VTIdx ),
>>	MaxLastHit is VTIdx - <<SetSizeOfLevel1-1>>,
>>	lru:lru( Case_Tagafter, VT, VTSet, VTIdx, S0, Hits, HitSets, MaxLastHit, <<SetSizeOfLevel1>> )
>> ),

%>> foreach set
%>> {
%>>	  CaseXX_0 = len of program + 1
%>>	  reverse foreach vytesn tags of this set but not more miss'es than N-1 (N = size of set)
%>>	  {
%>>	  	CaseXX_i < CaseXX_(i-1) 
%>>    	lru:lru( CaseXX_i, VytesnTag, ...hits, tags, levels ... )
%>>	  	i++;
%>>	  }
%>> }

% next cache level
% next set for this cache level
% lru( _5 )
( false 
; Case_5 = 1, _5 = Level1Set0IniTag2, Level1Set0_0 \ _5set sameset []  \/ Level1Set0IniTag1set \/ _0set
; Case_5 = 0, _5 = Level1Set0IniTag3, Level1Set0_0 \ _5set sameset []  \/ Level1Set0IniTag2set \/ Level1Set0IniTag1set \/ _0set
),

% next cache level
% next set for this cache level
% lru( _2 )
( false 
; Case_2 = 0, _2 = Level2Set0IniTag5, Level2Set0_0 \ _2set sameset []  \/ Level2Set0IniTag4set \/ Level2Set0IniTag3set \/ Level2Set0IniTag2set \/ Level2Set0IniTag1set
),


%вытесненные
indomain( _5 ),
indomain( _2 ),
%hit
indomain( _0 ),
indomain( _4 ),
%miss
indomain( _3 ),
indomain( _1 ),
labeling( Level1Set0 ),
labeling( Level2Set0 ),
numbers:random_result( _0x ),
numbers:random_result( _0y ),
numbers:random_result( _0z ),
numbers:random_result( _0c ),
true.

'ADD@overflow#0::main'( _, _1rd, _1rs, _1rt, _1rd, _1rs, _1rt) :- 
numbers:sizeof( _1rd, 64),
numbers:sizeof( _1rs, 64),
numbers:sizeof( _1rt, 64),
'ADD@overflow#0::2'(_1rd, _1rs, _1rt),
numbers:sizeof( _1temp, 33),
numbers:getbit( _0, _1rs, 64, 31 ),
numbers:getbits( _1, _1rs, 64, 31, 0 ),
numbers:concat( _2, _0, 1, _1, 32 ),
numbers:getbit( _3, _1rt, 64, 31 ),
numbers:getbits( _4, _1rt, 64, 31, 0 ),
numbers:concat( _5, _3, 1, _4, 32 ),
numbers:sum( _6, _2, _5, 33 ),
_1temp = _6,
'ADD@overflow#0::3'(_1rd, _1rs, _1rt, _1temp),
true.

'ADD@overflow#0::2'(_1rd, _1rs, _1rt) :-
'ADD@overflow#0::0'(_1rd, _1rs, _1rt), 'ADD@overflow#0::1'(_1rd, _1rs, _1rt).

'ADD@overflow#0::3'(_1rd, _1rs, _1rt, _1temp) :-
numbers:getbit( _7, _1temp, 33, 32 ),
numbers:getbit( _8, _1temp, 33, 31 ),
numbers:notequal( _7, _8, 1 ).

'ADD@overflow#0::0'(_1rd, _1rs, _1rt) :-
predicates:'WordValue'( _1rs, 64 ).

'ADD@overflow#0::1'(_1rd, _1rs, _1rt) :-
predicates:'WordValue'( _1rt, 64 ).

'LW@noexception#1::main'( _, _1rt, _1base, _1offset, _1rt, _1base, _1offset, _4, _5) :- 
numbers:sizeof( _1rt, 64),
numbers:sizeof( _1base, 64),
numbers:sizeof( _1offset, 16),
'LW@noexception#1::0'(_1rt, _1base, _1offset),
numbers:sizeof( _1addr, 32),
numbers:signExtend( _0, _1offset, 16, 32 ),
numbers:getbits( _1, _1base, 64, 31, 0 ),
numbers:sum( _2, _0, _1, 32 ),
_1addr = _2,
'LW@noexception#1::1'(_1rt, _1base, _1offset, _1addr),
numbers:getbits( [ _4 ], _1addr, 32, 31, 28 ),
numbers:getbits( [ _5 ], _1addr, 32, 31, 26 ),
true.

'LW@noexception#1::0'(_1rt, _1base, _1offset) :-
predicates:'WordValue'( _1base, 64 ).

'LW@noexception#1::1'(_1rt, _1base, _1offset, _1addr) :-
numbers:getbits( _3, _1addr, 32, 1, 0 ),
numbers:equal( _3, [ 0 ], 2 ).

'ADD@overflow#2::main'( _, _1rd, _1rs, _1rt, _1rd, _1rs, _1rt) :- 
numbers:sizeof( _1rd, 64),
numbers:sizeof( _1rs, 64),
numbers:sizeof( _1rt, 64),
'ADD@overflow#2::2'(_1rd, _1rs, _1rt),
numbers:sizeof( _1temp, 33),
numbers:getbit( _0, _1rs, 64, 31 ),
numbers:getbits( _1, _1rs, 64, 31, 0 ),
numbers:concat( _2, _0, 1, _1, 32 ),
numbers:getbit( _3, _1rt, 64, 31 ),
numbers:getbits( _4, _1rt, 64, 31, 0 ),
numbers:concat( _5, _3, 1, _4, 32 ),
numbers:sum( _6, _2, _5, 33 ),
_1temp = _6,
'ADD@overflow#2::3'(_1rd, _1rs, _1rt, _1temp),
true.

'ADD@overflow#2::2'(_1rd, _1rs, _1rt) :-
'ADD@overflow#2::0'(_1rd, _1rs, _1rt), 'ADD@overflow#2::1'(_1rd, _1rs, _1rt).

'ADD@overflow#2::3'(_1rd, _1rs, _1rt, _1temp) :-
numbers:getbit( _7, _1temp, 33, 32 ),
numbers:getbit( _8, _1temp, 33, 31 ),
numbers:notequal( _7, _8, 1 ).

'ADD@overflow#2::0'(_1rd, _1rs, _1rt) :-
predicates:'WordValue'( _1rs, 64 ).

'ADD@overflow#2::1'(_1rd, _1rs, _1rt) :-
predicates:'WordValue'( _1rt, 64 ).

'LW@noexception#3::main'( _, _1rt, _1base, _1offset, _1rt, _1base, _1offset, _4, _5) :- 
numbers:sizeof( _1rt, 64),
numbers:sizeof( _1base, 64),
numbers:sizeof( _1offset, 16),
'LW@noexception#3::0'(_1rt, _1base, _1offset),
numbers:sizeof( _1addr, 32),
numbers:signExtend( _0, _1offset, 16, 32 ),
numbers:getbits( _1, _1base, 64, 31, 0 ),
numbers:sum( _2, _0, _1, 32 ),
_1addr = _2,
'LW@noexception#3::1'(_1rt, _1base, _1offset, _1addr),
numbers:getbits( [ _4 ], _1addr, 32, 31, 28 ),
numbers:getbits( [ _5 ], _1addr, 32, 31, 26 ),
true.

'LW@noexception#3::1'(_1rt, _1base, _1offset, _1addr) :-
numbers:getbits( _3, _1addr, 32, 1, 0 ),
numbers:equal( _3, [ 0 ], 2 ).

'LW@noexception#3::0'(_1rt, _1base, _1offset) :-
predicates:'WordValue'( _1base, 64 ).

