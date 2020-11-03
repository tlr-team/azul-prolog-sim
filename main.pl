
:- [predicates].

% Prepare board and call main loop
go :- 
    start_game,
    loop,
    print_winner.

% check winning condition
loop :- 
    game_end.

% round play
loop :- 
    play_game,
    loop.

% main game loop
play_game :-
    game_end, !.

play_game :-
    refill_round,
    play_round,
    calc_scores,
    play_game.

% empty board and round end
play_round :- 
    board_empty, !.

% each player makes its move
play_round :-
    sort_players([1,2,3,4], Order),
    my_member(PlayerNumber, Order),
    player_move(PlayerNumber),
    print_player(PlayerNumber).





%prints player state
print_player(PlayerNumber):-
    player(PlayerNumber, Score, Pieces, Board, Table, Floor),
    write("Player "), write(PlayerNumber), nl, nl,
    print_pieces(Pieces), nl, nl,
    print_table(Table), nl, nl
    print_floor(Floor), nl.

print_floor(Floor) :-
    len_count(Floor, Pieces),
    write (" Floor: "), write(Pieces), write(" pieces").

print_table(Table) :-
    my_member((Row, Pieces, Color), Table),
    print_row(Row, Pieces, Color), nl.

print_row(Row, _, none) :-
    write(" Row "), write(Row), write(":"),
    write(" has no pieces"), !.

print_row(Row, Pieces, _):-
    write(" Row "), write(Row), write(":"),
    my_member(Piece, Pieces),
    write(" "), write(Piece), write(" ").
    
    

print_pieces(Pieces) :-
    default_board(T),
    my_member(Row, T), nl,
    my_member((X,Y,Color), Row),
    print_one(X,Y,Color, Pieces).

print_one(X, Y, Color, Pieces) :-
    my_member((X,Y), Pieces),
    write(" "), write(Color), write(" "), !.

print_one(_, _, _, _) :-
    write(" "), write("   -   "), write(" ").
    

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
    factory_fill(FN,PPF),
    %Special piece location.
    special(Location),
    retract(special(Location)),
    assert(special(middle)).

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