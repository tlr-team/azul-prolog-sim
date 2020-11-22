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

complete_row_puntuation(Pieces, Row, 2) :-
    completed_row(Pieces, Row).

complete_row_puntuation(_, _, 0).

completed_row(Pieces, Row) :-
    my_member((Row,1), Pieces),
    my_member((Row,2), Pieces),
    my_member((Row,3), Pieces),
    my_member((Row,4), Pieces),
    my_member((Row,5), Pieces).

complete_column_puntuation(Pieces, Column, 7) :-
    my_member((1,Column), Pieces),
    my_member((2,Column), Pieces),
    my_member((3,Column), Pieces),
    my_member((4,Column), Pieces),
    my_member((5,Column), Pieces).

complete_column_puntuation(_, _, 0).

%take all the pieces of the same type from one factory
take_color(Factory, Color, Pieces, Rest) :-
    take_color_(Factory, Color, Pieces, Rest, [], []).

take_color_([], _, Pieces, Rest, Pieces, Rest).

take_color_([A|B], A, Pieces, Rest, PiecesAcc, RestAcc) :-
    take_color_(B, A, Pieces, Rest, [A|PiecesAcc], RestAcc), !.

take_color_([A|B], Color, Pieces, Rest, PiecesAcc, RestAcc) :-
    take_color_(B, Color, Pieces, Rest, PiecesAcc, [A|RestAcc]).

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

player_move(PlayerNumber) :-
    player(PlayerNumber, _, _, Board, Table, Floor),
    % selected all playable moves
    all_possible_moves(Table, Board, Moves),
    % take the best possible move ( it is a factory )
    best_player_move(Board, Table, Floor, Moves, Best_Move),
    % plays the selected move
    play_move(PlayerNumber, Best_Move, Resto),
    % change the game logic
    update_game(Best_Move, Resto, PlayerNumber).

% triunfa si (A. List, col) es miembre de table pero no esta completo
full_row((A, List, Col), Table) :-
    my_member((A, List, Col), Table),
    not(member_count(Col, List, A)).

play_move(PlayerNumber, (-1, Color, Factory), Resto) :-
    nogoodmove,
    retract(nogoodmove),
    player(PlayerNumber, Score, Pieces, Board, Table, Floor),
    take_color(Factory, Color, Fichas, Resto),
    write("El jugador "), write(PlayerNumber), write(" selecciona la factoria "), write(Factory),nl,
    write("y descarta el color "), write(Color), nl, nl,
    my_concat(Fichas, Floor, NewFloor),
    retract(player(PlayerNumber, Score, Pieces, Board, Table, Floor)),
    assert(player(PlayerNumber, Score, Pieces, Board, Table, NewFloor)),!.

play_move(PlayerNumber, (Row, Color, Factory), Resto) :-
    player(PlayerNumber, Score, Pieces, Board, Table, Floor),
    take_color(Factory, Color, Fichas, Resto),
    %write("--------------------"),write(Fichas),write(Resto),nl,
    write("El jugador "), write(PlayerNumber), write(" selecciona la factoria "), write(Factory),nl,
    write("Selecciona la fila "), write(Row), write(" y toma el color "), write(Color), nl, nl,
    insert_pieces_into_player_table(Fichas, Table, Floor, Color, Row, NewTable, NewFloor),
    retract(player(PlayerNumber, Score, Pieces, Board, Table, Floor)),
    assert(player(PlayerNumber, Score, Pieces, Board, NewTable, NewFloor)).
    %write("result"),write(NewTable), write(NewFloor),nl.
    

% read declaration
insert_pieces_into_player_table(Fichas, Table, Floor, Color, Row, NewTable, NewFloor) :-
    my_remove((Row, Acc, _), Table, MiddleTable),
    add_pieces(Fichas, Acc, Row, Color, NewAcc, Resto),
    my_insert((Row, NewAcc, Color), MiddleTable, NewTable),
    my_concat(Resto, Floor, NewFloor).

% fills A Table Row
add_pieces([], Actuales, _, _, Actuales, []).

add_pieces(Fichas, Actuales, Row, Color, Actuales, Fichas) :-
    member_count(Color, Actuales, Row).

add_pieces([A|B], Actuales, Row, Color, Changed, Rest) :-
    my_insert(A, Actuales, NewActuales),
    add_pieces(B, NewActuales, Row, Color, Changed, Rest).

update_game((_,_,Factory), Resto, _):-
    factories(Facts),
    my_remove(Factory, Facts, Result),
    update_resto(Resto),
    retract(factories(Facts)),
    assert(factories(Result)),!.

update_game(_, Resto, PlayerNumber) :-
    middle(Medio),
    retract(middle(Medio)),
    assert(middle(Resto)),
    update_middle_piece(PlayerNumber). % se necesita para el orden de los players en cada ronda

update_resto([]).

update_resto(Resto) :-
    middle(Medio),
    my_concat(Resto, Medio, Result),
    retract(middle(Medio)),
    assert(middle(Result)).   
    

%moves the special piece
update_middle_piece(PlayerNumber) :-
    special(middle),
    retract(special(middle)),
    assert(special(PlayerNumber)), !.

update_middle_piece(_).


% there is no good move
best_player_move(_, _, _, [], (-1, Color, Choice)) :-
    assert(nogoodmove),
    factories(Factories),
    middle(Middle),
    my_insert(Middle,Factories, Concat),
    random_select(Choice, Concat, _),
    random_select(Color, Choice, _).

best_player_move(_, _, _, Moves, Choice):-
    %best_player_move(Board, Table, Floor, Moves, Factory):-
    %random select a move.
    %TODO take the best move.
    random_select(Choice, Moves, _).




% seleccionar todas los pares (fila, color) que son jugables.
all_possible_moves(Table, Board, Moves) :-
    findall((Row, Color, Factory), possible_row_color_and_factory(Row, Color, Factory, Table, Board), Moves).

possible_row_color_and_factory(Row, Color, Factory, Table, Board) :-
    colors(Colors),
    my_member(Color, Colors),
    row(Rows),
    my_member(Row, Rows),
    playable_row(Row, Table),
    playable_color(Row, Color, Table, Board),
    playable_factory(Factory, Color).

playable_factory(Factory, Color) :-
    factories(Factories),
    my_member(Factory, Factories),
    my_member(Color, Factory).

playable_factory(Factory, Color) :-
    middle(Factory),
    my_member(Color, Factory).

playable_row(Row, Table) :-
    my_member((Row, Pieces, Color), Table),
    not(member_count(Color, Pieces, Row)).

% no hay color seleccionado
playable_color(Row, Color, Table, Board) :-
    my_member((Row, _, none), Table),
    my_member((Row, _, Color), Board), !.

% ya existe un color seleccionado
playable_color(Row, Color, Table, _) :-
    my_member((Row, _, Color), Table).