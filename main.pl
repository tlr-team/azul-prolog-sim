
:- ['Logic/predicates'].
:- ['Logic/utils'].

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