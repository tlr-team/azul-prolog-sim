

my_member(X, [X|_]).
my_member(X, [_|Y]) :- my_member(X,Y).

% check for a M times member
member_count(X, Y, 0).
member_count(X, [X|Y], M) :-
    N is M - 1,
    member_count(X,Y,N).
member_count(X, [_|Y], M) :- 
    member_count(X, Y, M).

my_concat([],X,X).
my_concat([X|L],Y,[X|Z]) :- my_concat(L,Y,Z).

my_insert(X,Y,[X|Y]).
my_insert(X,[A|Y],[A|Z]) :- my_insert(X,Y,Z).

my_remove(X,Y,Z) :- my_insert(X,Z,Y).




