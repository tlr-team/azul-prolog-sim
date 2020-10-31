
:- [predicates].

% Prepare board and call main loop
go :- 
    start_game,
    loop.

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
    assert(special(middle)),
