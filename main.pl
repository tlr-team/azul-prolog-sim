
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
    board_empty.

% each player makes its move
play_round :-
    player1_move,
    player2_move,
    player3_move,
    player4_move,
    play_round.