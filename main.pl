
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
    play_round,
    loop.

% empty board and round end
play_round :- 
    (game_end ; board_empty), !.

% each player makes its move
play_round :-
    sort_players([1,2,3,4], Order),
    my_member(PlayerNumber, Order),
    player_move(PlayerNumber),
    print_player(PlayerNumber).

%prints player state
print_player(PlayerNumber):-
    true.
