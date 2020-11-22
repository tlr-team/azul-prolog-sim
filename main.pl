
:- [predicates].
:- [utils].

% Prepare board and call main loop
go :- 
    start_game,
    play_game,
    update_player_scores,
    print_winner, !.

% check winning condition
play_game :-
    game_end, !.

% main loop
play_game :-
    refill_round,
    print_no_player,
    play_round,
    calc_scores,
    play_game.

% empty board and round end
play_round :- 
    board_empty, !.

% each player makes its move
play_round :-
    sort_players([1,2,3,4], Order),
    play_complete_round(Order).

play_complete_round(_) :-
    board_empty, !.

play_complete_round(Order) :-
    play_round_(Order),
    play_complete_round(Order).

play_round_([]).

play_round_(_) :-
    board_empty, !.

play_round_([Player|Rest]) :-
    player_move(Player),
    print_no_player,
    print_player(Player),
    play_round_(Rest).  

board_empty :-
    middle([]),
    factories([]).


% ============================ score area ==========================================

%each player score
calc_scores :-
    calc_scores_([1,2,3,4]).

calc_scores_([]).

calc_scores_([Player|Tail]) :-
    player(Player, Score, Pieces, Board, Table, Floor),
    % select full rows
    findall((Row, Color), table_row_ready(Row, Color, Table), Targets),
    % take the piece from the board
    update_board_and_pieces(Targets, Board, Pieces, NewBoard, NewPieces, SelectedPieces),
    % reset table
    update_table(Targets, Table, NewTable),
    % calc score with new pieces
    calc_player_score(SelectedPieces, Pieces, Floor, Score, NewScore),
    % puntuation >= 0
    my_max(NewScore, 0, RoundedScore),
    % update player
    retract(player(Player, Score, Pieces, Board, Table, Floor)),
    assert(player(Player, RoundedScore, NewPieces, NewBoard, NewTable, [])),
    calc_scores_(Tail).

calc_player_score([], _, Floor, Score, NewScore) :-
    len_count(Floor, Count),
    floor(Count, Value),
    NewScore is Score + Value.

calc_player_score([(Row, Column)|Tail], Pieces, Floor, Score, NewScore) :-
    calc_score((Row, Column), Pieces, Value),
    my_insert((Row, Column), Pieces, NextPieces),
    NextScore is Score + Value,
    calc_player_score(Tail, NextPieces, Floor, NextScore, NewScore).


update_table([], Table, Table).

update_table([(Row, Color)|Tail], Table, NewTable) :-
    my_remove((Row, _, Color), Table, MiddleTable),
    my_insert((Row, [], none), MiddleTable, NextTable),
    update_table(Tail, NextTable, NewTable).

update_board_and_pieces(Targets, Board, Pieces, NewBoard, NewPieces, SelectedPieces) :- 
    update_board_and_pieces_(Targets, Board, Pieces, NewBoard, NewPieces, [], SelectedPieces).

update_board_and_pieces_([], Board, Pieces, Board, Pieces, SelectedPieces, SelectedPieces).

update_board_and_pieces_([(Row, Color) | Tail], Board, Pieces, NewBoard, NewPieces, SelectedPiecesAcc, SelectedPieces) :-
    board_remove(Row, Color, Board, Piece, NextBoard),
    my_insert(Piece, Pieces, NextPieces),
    my_insert(Piece, SelectedPiecesAcc, NextSelectedPieces),
    update_board_and_pieces_(Tail, NextBoard, NextPieces, NewBoard, NewPieces, NextSelectedPieces, SelectedPieces).

board_remove(Row, Color, Board, (Row, Column), NewBoard) :-
    my_remove((Row, Column, Color), Board, NewBoard).

table_row_ready(Row, Color, Table) :-
    my_member((Row, Pieces, Color), Table),
    len_count(Pieces, Row).

% ============================================== end score area =====================================================

% ============================================== print area =========================================================

print_winner :-
    print_winners([1,2,3,4]).

print_winners([]).

print_winners([Player|Tail]) :-
    player(Player, Score, _, _, _, _),
    write("El jugador "), write(Player), write(" obtuvo "), write(Score), write(" puntos."),nl,
    print_winners(Tail).

print_no_player :-
    print_factories, nl,
    print_middle, nl, nl.

print_factories :-
    factories(F),
    print_factories_(F).

print_factories_([]).

print_factories_([Factory | Tail]) :-
    print_factory(Factory), nl,
    print_factories_(Tail).

print_factory(Factory) :-
    write(" Factory: "),
    print_values(Factory).

print_middle :-
    middle(Pieces),
    write(" Middle: "),
    print_values(Pieces).

%prints player state
print_player(PlayerNumber):-
    player(PlayerNumber, _, Pieces, _, Table, Floor),
    write("Player "), write(PlayerNumber), nl, nl,
    print_pieces(Pieces),
    print_table(Table), nl,
    print_floor(Floor), nl, !.

print_floor(Floor) :-
    len_count(Floor, Pieces),
    write(" Floor: "), write(Pieces), write(" pieces"), nl.

print_table(Table) :-
    my_member((1, Pieces1, Color1), Table),
    print_row(1, Pieces1, Color1), nl,
    my_member((2, Pieces2, Color2), Table),
    print_row(2, Pieces2, Color2), nl,
    my_member((3, Pieces3, Color3), Table),
    print_row(3, Pieces3, Color3), nl,
    my_member((4, Pieces4, Color4), Table),
    print_row(4, Pieces4, Color4), nl,
    my_member((5, Pieces5, Color5), Table),
    print_row(5, Pieces5, Color5), nl.

print_row(Row, _, none) :-
    write(" Row "), write(Row), write(":"),
    write(" has no pieces"), !.

print_row(Row, Pieces, _):-
    write(" Row "), write(Row), write(":"),
    print_values(Pieces).

print_pieces([]).

print_pieces(Pieces) :-
    default_board(T),
    print_pieces_(T, Pieces), nl.

print_pieces_([], _).

print_pieces_([Head|Tail], Pieces) :-
    print_one(Head, Pieces),
    print_new_line(Head),
    print_pieces_(Tail, Pieces).

print_new_line((_, 5, _)) :-
    nl,!.

print_new_line(_).

print_one((X, Y, Color), Pieces) :-
    my_member((X,Y), Pieces),
    write(" "), write(Color), write(" "), !.

print_one(_, _) :-
    write(" "), write("   -   "), write(" ").

% ============================================== end print area =====================================================

refill_round :-
    % release previos bag
    bag(T),
    retract(bag(T)),
    % retake pieces from gab
    default_bag(L),
    assert(bag(L)),
    %factory list
    factories_number(FN),
    pieces_per_factory(PPF),
    factory_fill(FN,PPF).
    %Special piece location.
    % special(Location),
    % retract(special(Location)),
    % assert(special(middle)).

game_end :-
    % a player fill an entire row
    player(1, _, Pieces1, _, _, _),
    player(2, _, Pieces2, _, _, _),
    player(3, _, Pieces3, _, _, _),
    player(4, _, Pieces4, _, _, _),
    (   completed_some_row(Pieces1);
        completed_some_row(Pieces2);
        completed_some_row(Pieces3);
        completed_some_row(Pieces4) ).
    
% check all rows for completion
completed_some_row(Pieces):-
    completed_row(Pieces, 1);
    completed_row(Pieces, 2);
    completed_row(Pieces, 3);
    completed_row(Pieces, 4);
    completed_row(Pieces, 5).

winner(Player, Score) :-
    my_member(Player, [1,2,3,4]),
    player(Player, Score, _, _, _, _),
    max_score(Score).

max_score(Score) :-
    my_member(Player, [1,2,3,4]),
    player(Player, Score2, _, _, _, _),
    Score >= Score2.

update_player_scores :-
    update_player_score([1,2,3,4]).

update_player_score([]).

update_player_score([Player| Tail]) :-
    player(Player, Score, Pieces, Board, Table, Floor),
    complete_column_puntuation(Pieces, 1, Score1),
    complete_column_puntuation(Pieces, 2, Score2),
    complete_column_puntuation(Pieces, 3, Score3),
    complete_column_puntuation(Pieces, 4, Score4),
    complete_column_puntuation(Pieces, 5, Score5),
    complete_row_puntuation(Pieces, 1, Score11),
    complete_row_puntuation(Pieces, 2, Score22),
    complete_row_puntuation(Pieces, 3, Score33),
    complete_row_puntuation(Pieces, 4, Score44),
    complete_row_puntuation(Pieces, 5, Score55),
    complete_color_puntuation(Pieces, ScoreC),
    NewScore is Score + Score1 + Score2 + Score3 + Score4 + Score5 + Score11 + Score22 + Score33 + Score44 + Score55 + ScoreC,
    retract(player(Player, Score, Pieces, Board, Table, Floor)),
    assert(player(Player, NewScore, Pieces, Board, Table, Floor)),
    update_player_score(Tail).