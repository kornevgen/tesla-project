:- module( main ).
:- lib( ic ).
:- use_module( numbers ).

:- export go/2.
go( _, _x ) :-
numbers:number2number( _x, _1x, 10000),
numbers:sizeof( _1x, 10000),
'0'( _1x ) ,
numbers:random_result( _1x ),
numbers:number2number( _x, _1x, 10000 ),
true.

'0'( _1x )  :-
numbers:getbit( _0, _1x, 10000, 0 ),
numbers:getbit( _1, _1x, 10000, 1 ),
numbers:greaterUnsigned( _0, _1, 1 ).
