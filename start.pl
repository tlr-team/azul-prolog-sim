:- use_module(library(random),[random_select/3]).
:- dynamic bag/1.
:- dynamic factories/1.
:- dynamic special/1.
:- [utils].
:- [static].

start_game :-
    %pieces_bag
    default_bag(L),
    assert(bag(L)),
    %factory list
    assert(factories([])),
    factory_fill(9,4),
    %middle board
    assert(middle([])),
    %Special piece location.
    assert(special(middle)),
    default_board(Board),
    default_table(Table),
    %player(Number, Score, Pieces, board, table, floor)
    assert(player(1,0,[], Board, Table, [])),
    assert(player(2,0,[], Board, Table, [])),
    assert(player(3,0,[], Board, Table, [])),
    assert(player(4,0,[], Board, Table, [])).
   



% Result is a new Factory (color list)
factory_gen(Size, Result) :-
    factory_gen(Size, [], Result).

factory_gen(0, Result, Result).

factory_gen(Size, Result, Acc) :-
    bag(L),
    random_select(S, L, R),
    retract(bag(L)),
    assert(bag(R)),
    NSize is Size - 1,
    factory_gen(NSize, [S|Result], Acc), !.

% Fills factories/1 rule.
factory_fill(Number, Size) :-
    factory_fill(Number, Size, []).

factory_fill(0, _, Factories) :-
    factories(L),
    retract(factories(L)),
    assert(factories(Factories)), !.

factory_fill(Number, Size, L) :-
    factory_gen(Size, F),
    Nnumber is Number - 1,
    factory_fill(Nnumber, Size, [F|L]).

same_row((X,_),(X,_)).

same_column((_,Y),(_,Y)).

same_point((A,B),(A,B)).

% wins if (A,B) is in the same row as (C,D) in L
direct_connected_row((A,B),(C,D),L) :-
    same_row((A,B),(C,D)),
    my_member((A,B),L),
    connected((A,B),(C,D)).

connected_by_row((A,B), (D,C), L) :-
    direct_connected_row((A,B), (D,C), L).

connected_by_row((T,Z), (D,C), L) :-
    direct_connected_row((A,B),(D,C),L),
    my_remove((A,B),L,NL),
    connected_by_row((T,Z),(A,B),NL).

% wins if (A,B) is in the same column as (C,D) in L
direct_connected_column((A,B),(C,D),L) :-
    same_column((A,B),(C,D)),
    my_member((A,B),L),
    connected((A,B),(C,D)).

connected_by_column((A,B), (D,C), L) :-
    direct_connected_column((A,B), (D,C), L).

connected_by_column((T,Z), (D,C), L) :-
    direct_connected_column((A,B),(D,C),L),
    my_remove((A,B),L,NL),
    connected_by_column((T,Z),(A,B),NL).
    
% calculates the score of add a piece in the position x,y on the players board.
calc_score((X,Y), Pieces, Val) :-
    findall((A,B), connected_by_column((A,B), (X,Y), Pieces), CP),
    findall((A,B), connected_by_row((A,B), (X,Y), Pieces), RP),
    my_concat(CP, RP, U),
    my_count(U, Val).