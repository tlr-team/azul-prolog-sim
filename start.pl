:- use_module(library(random),[random_select/3]).
:- dynamic bag/1.
:- dynamic factories/1.
:- dynamic special/1.
:- [utils].

%player pieces board connected relation
% 1,1
% connected((1,1),(1,1)).
connected((1,1),(1,2)).
connected((1,1),(2,1)).
% 1,2
connected((1,2),(1,1)).
% connected((1,2),(1,2)).
connected((1,2),(1,3)).
connected((1,2),(2,2)).
% 1,3
connected((1,3),(1,2)).
% connected((1,3),(1,3)).
connected((1,3),(1,4)).
connected((1,3),(2,3)).
% 1,4
connected((1,4),(1,3)).
% connected((1,4),(1,4)).
connected((1,4),(1,5)).
connected((1,4),(2,4)).
% 1,5
connected((1,5),(1,4)).
% connected((1,5),(1,5)).
connected((1,5),(2,5)).
% 2,1
connected((2,1),(1,1)).
% connected((2,1),(2,1)).
connected((2,1),(2,2)).
connected((2,1),(3,1)).
% 2,2
connected((2,2),(1,2)).
connected((2,2),(2,1)).
% connected((2,2),(2,2)).
connected((2,2),(2,3)).
connected((2,2),(3,2)).
% 2,3
connected((2,3),(1,3)).
connected((2,3),(2,2)).
% connected((2,3),(2,3)).
connected((2,3),(2,4)).
connected((2,3),(3,3)).
% 2,4
connected((2,4),(1,4)).
connected((2,4),(2,3)).
% connected((2,4),(2,4)).
connected((2,4),(2,5)).
connected((2,4),(3,4)).
% 2,5
connected((2,5),(1,5)).
connected((2,5),(2,4)).
% connected((2,5),(2,5)).
connected((2,5),(3,5)).
% 3,1
connected((3,1),(2,1)).
% connected((3,1),(3,1)).
connected((3,1),(4,1)).
connected((3,1),(3,2)).
% 3,2
connected((3,2),(3,1)).
% connected((3,2),(3,2)).
connected((3,2),(3,3)).
connected((3,2),(2,2)).
connected((3,2),(4,2)).
% 3,3
connected((3,3),(3,2)).
% connected((3,3),(3,3)).
connected((3,3),(3,4)).
connected((3,3),(2,3)).
connected((3,3),(4,3)).
% 3,4
connected((3,4),(3,3)).
% connected((3,4),(3,4)).
connected((3,4),(3,5)).
connected((3,4),(2,4)).
connected((3,4),(4,4)).
% 3,5
connected((3,5),(3,4)).
% connected((3,5),(3,5)).
connected((3,5),(4,5)).
connected((3,5),(2,5)).
% 4,1
% connected((4,1),(4,1)).
connected((4,1),(3,1)).
connected((4,1),(5,1)).
connected((4,1),(4,2)).
% 4,2
connected((4,2),(4,1)).
% connected((4,2),(4,2)).
connected((4,2),(4,3)).
connected((4,2),(3,2)).
connected((4,2),(5,2)).
% 4,3
connected((4,3),(4,2)).
% connected((4,3),(4,3)).
connected((4,3),(4,4)).
connected((4,3),(3,3)).
connected((4,3),(5,3)).
% 4,4
connected((4,4),(4,3)).
% connected((4,4),(4,4)).
connected((4,4),(4,5)).
connected((4,4),(3,4)).
connected((4,4),(5,4)).
% 4,5
connected((4,5),(4,4)).
% connected((4,5),(4,5)).
connected((4,5),(3,5)).
connected((4,5),(5,5)).
% 5,1
connected((5,1),(4,1)).
% connected((5,1),(5,1)).
connected((5,1),(5,2)).
% 5,2
connected((5,2),(5,1)).
% connected((5,2),(5,2)).
connected((5,2),(5,3)).
connected((5,2),(4,2)).
% 5,3
connected((5,3),(5,2)).
% connected((5,3),(5,3)).
connected((5,3),(5,4)).
connected((5,3),(4,3)).
% 5,4
connected((5,4),(5,3)).
% connected((5,4),(5,4)).
connected((5,4),(5,5)).
connected((5,4),(4,4)).
% 5,5
connected((5,5),(5,4)).
% connected((5,5),(5,5)).
connected((5,5),(4,5)).

%factory list
factories([]).

%Special piece location.
special(middle).

%pieces bag
bag([azul, azul, azul, azul, azul,
    azul, azul, azul, azul, azul,
    azul, azul, azul, azul, azul,
    azul, azul, azul, azul, azul,
    naranja, naranja, naranja, naranja, naranja,
    naranja, naranja, naranja, naranja, naranja,
    naranja, naranja, naranja, naranja, naranja,
    naranja, naranja, naranja, naranja, naranja,
    rojo, rojo, rojo, rojo, rojo,
    rojo, rojo, rojo, rojo, rojo,
    rojo, rojo, rojo, rojo, rojo,
    rojo, rojo, rojo, rojo, rojo,
    negro, negro, negro, negro, negro,
    negro, negro, negro, negro, negro,
    negro, negro, negro, negro, negro,
    negro, negro, negro, negro, negro,
    blanco, blanco, blanco, blanco, blanco,
    blanco, blanco, blanco, blanco, blanco,
    blanco, blanco, blanco, blanco, blanco,
    blanco, blanco, blanco, blanco, blanco]).

default_board([
        [
            (1,1,azul),
            (1,2,naranja),
            (1,3,rojo),
            (1,4,negro),
            (1,5,blanco)],
        [
            (2,1,blanco),
            (2,2,azul),
            (2,3,naranja),
            (2,4,rojo),
            (2,5,negro)],
        [
            (3,1,negro),
            (3,2,blanco),
            (3,3,azul),
            (3,4,naranja),
            (3,5,rojo)],
        [
            (4,1,rojo),
            (4,2,negro),
            (4,3,blanco),
            (4,4,azul),
            (4,5,naranja)],
        [
            (5,1,naranja),
            (5,2,rojo),
            (5,3,negro),
            (5,4,blanco),
            (5,5,azul)]]).


% amount of pieces on the players floor
floor(1, -1).
floor(2, -2).
floor(3, -4).
floor(4, -6).
floor(5, -8).
floor(6, -11).
floor(7, -14).
floor(X, -14).

% Result is a new Factory (color list)
factory_gen(Size, Result) :-
    factory_gen(Size, [], Result).

factory_gen(0, Result, Result).

factory_gen(Size, Result, Acc) :-
    bag(L),
    random_select(S, L, R),
    retract(bag(L)),
    assert(bag(R)),
    NSize is Size - 1,
    factory_gen(NSize, [S|Result], Acc), !.

% Fills factories/1 rule.
factory_fill(Number, Size) :-
    factory_fill(Number, Size, []).

factory_fill(0, Size, Factories) :-
    factories(L),
    retract(factories(L)),
    assert(factories(Factories)), !.

factory_fill(Number, Size, L) :-
    factory_gen(Size, F),
    Nnumber is Number - 1,
    factory_fill(Nnumber, Size, [F|L]).

same_row((X,_),(X,_)).

same_column((_,Y),(_,Y)).

same_point((A,B),(A,B)).

% wins if (A,B) is in the same row as (C,D) in L
direct_connected_row((A,B),(C,D),L) :-
    same_row((A,B),(C,D)),
    my_member((A,B),L),
    connected((A,B),(C,D)).

connected_by_row((A,B), (D,C), L) :-
    direct_connected_row((A,B), (D,C), L).

connected_by_row((T,Z), (D,C), L) :-
    direct_connected_row((A,B),(D,C),L),
    my_remove((A,B),L,NL),
    connected_by_row((T,Z),(A,B),NL).

% wins if (A,B) is in the same column as (C,D) in L
direct_connected_column((A,B),(C,D),L) :-
    same_column((A,B),(C,D)),
    my_member((A,B),L),
    connected((A,B),(C,D)).

connected_by_column((A,B), (D,C), L) :-
    direct_connected_column((A,B), (D,C), L).

connected_by_column((T,Z), (D,C), L) :-
    direct_connected_column((A,B),(D,C),L),
    my_remove((A,B),L,NL),
    connected_by_column((T,Z),(A,B),NL).
    
% calculates the score of add a piece in the position x,y on the players board.
calc_score((X,Y), Pieces, Val) :-
    findall((A,B), connected_by_column((A,B), (X,Y), Pieces), CP),
    findall((A,B), connected_by_row((A,B), (X,Y), Pieces), RP),
    my_concat(CP, RP, U),
    my_count(U, Val).