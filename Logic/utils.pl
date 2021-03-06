

my_member(X, [X|_]).
my_member(X, [_|Y]) :- my_member(X,Y).

% check for a M times member
member_count(_, _, 0).
member_count(X, [X|Y], M) :-
    N is M - 1,
    member_count(X,Y,N).
member_count(X, [_|Y], M) :- 
    member_count(X, Y, M).

len_count(X, Result) :-
    len_count_(X, 0, Result).

len_count_([], Acc, Acc).

len_count_([ _ | A], Acc, Result) :-
    NewAcc is Acc + 1,
    len_count_(A, NewAcc, Result).


my_concat([],X,X).
my_concat([X|L],Y,[X|Z]) :- my_concat(L,Y,Z).

my_insert(X,Y,[X|Y]).
my_insert(X,[A|Y],[A|Z]) :- my_insert(X,Y,Z).

my_remove(X,Y,Z) :- my_insert(X,Z,Y).

my_count(L, R) :-
    my_count(L, 0, R).

my_count([], A, A).

my_count([_|Y], A, R) :-
    Newa is A + 1,
    my_count(Y, Newa, R).

my_subllist(L, T) :-
    my_member(X,L),
    my_member(X,T).

my_max(X,Y,X) :-
    X >= Y.

my_max(_,Y, Y).
    
print_values([]).

print_values([Head | Tail]) :-
    write(" "), write(Head), write(" "),
    print_values(Tail).
    

