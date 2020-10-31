
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
    true.

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