
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
    print_no_player,
    print_player(PlayerNumber).


print_no_player :-
    print_factories,
    print_middle, nl.

print_factories :-
    factories(F),
    print_fact_(F).

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
    player(PlayerNumber, Score, Pieces, Board, Table, Floor),
    write("Player "), write(PlayerNumber), nl, nl,
    print_pieces(Pieces), nl, nl,
    print_table(Table), nl, nl,
    print_floor(Floor), nl.

print_floor(Floor) :-
    len_count(Floor, Pieces),
    write(" Floor: "), write(Pieces), write(" pieces").

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
    
print_pieces(Pieces) :-
    default_board(T),
    print_pieces_(T, Pieces).

print_pieces_([], _).

print_pieces_([Head|Tail], Pieces) :-
    print_row_pieces(Head, Pieces), nl,
    print_pieces_(Tail, Pieces).

print_row_pieces([], _).

print_row_pieces([(X,Y,Color)|Tail], Pieces) :-
    print_one(X,Y,Color, Pieces),
    print_row_pieces(Tail, Pieces).

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