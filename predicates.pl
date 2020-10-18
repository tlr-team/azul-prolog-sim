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
    factories_number(FN),
    pieces_per_factory(PPF),
    factory_fill(FN,PPF),
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

complete_row(Pieces, Row, Result) :-
    my_member((Row,1), Pieces),
    my_member((Row,2), Pieces),
    my_member((Row,3), Pieces),
    my_member((Row,4), Pieces),
    my_member((Row,5), Pieces),
    Result is 2.

complete_row(_, _, 0).

complete_column(Pieces, Column, Result) :-
    my_member((1,Column), Pieces),
    my_member((2,Column), Pieces),
    my_member((3,Column), Pieces),
    my_member((4,Column), Pieces),
    my_member((5,Column), Pieces),
    Result is 7.

complete_column(_, _, 0).

%take all the pieces of the same type from one factory
take_color(Factory, Color, Pieces, Rest) :-
    take_color_(Factory, Color, Pieces, Rest, [], []).

take_color_([], Color, Pieces, Rest, Pieces, Rest).

take_color_([A|B], A, Pieces, Rest, PiecesAcc, RestAcc) :-
    take_color_(B, A, Pieces, Rest, [A|PiecesAcc], RestAcc), !.

take_color_([A|B], Color, Pieces, Rest, PiecesAcc, RestAcc) :-
    take_color_(B, Color, Pieces, Rest, PiecesAcc, [A|RestAcc]).

take_all_moves(Factory, Result) :-
    take_all_moves_(Factory, Result, []).

take_all_moves_([], Result, Result).

take_all_moves_(Factory, Result, Acc) :-
    my_member(X, Factory),
    take_color(Factory, X, Pieces, Rest),
    not(my_member((Pieces, Rest), Acc)),

my_max([], 0).

my_max(Lista, Res) :-
    my_member(X, Lista),
    max_(Lista, X),
    Res is X, !.

max_([], _).

max_([X | Tail], Max) :-
    Max >= X,
    max_(Tail, Max).

% special(player) list sort. (sort_players([1,2,3,4], R))
sort_players(L, Result) :-
    sort_players_([], L, Result).

sort_players_(Head, [], Head).

sort_players_(Head, [Term | Tail], Result) :-
    special(Term),
    retract(special(Term)), 
    assert(special(middle)), % Special piece back into the middle.
    my_concat([Term | Tail], Head, Result), !.

sort_players_(Head, [Term | Tail], Result) :-
    my_concat(Head, [Term], NHead),
    sort_players_(NHead, Tail, Result).

player_move(Player) :-
    player(Player, Score, Pieces, Board, Table, Floor),
    select_row(Fila).

    
    