:- use_module(library(random),[random_select/3]).
:- dynamic bag/1.
:- dynamic factories/1.
:- dynamic special/1.

%player pieces board connected relation
% 1,1
connected((1,1),(1,1)).
connected((1,1),(1,2)).
connected((1,1),(2,1)).
% 1,2
connected((1,2),(1,1)).
connected((1,2),(1,2)).
connected((1,2),(1,3)).
connected((1,2),(2,2)).
% 1,3
connected((1,3),(1,2)).
connected((1,3),(1,3)).
connected((1,3),(1,4)).
connected((1,3),(2,3)).
% 1,4
connected((1,4),(1,3)).
connected((1,4),(1,4)).
connected((1,4),(1,5)).
connected((1,4),(2,4)).
% 1,5
connected((1,5),(1,4)).
connected((1,5),(1,5)).
connected((1,5),(2,5)).
% 2,1
connected((2,1),(1,1)).
connected((2,1),(2,1)).
connected((2,1),(2,2)).
connected((2,1),(3,1)).
% 2,2
connected((2,2),(1,2)).
connected((2,2),(2,1)).
connected((2,2),(2,2)).
connected((2,2),(2,3)).
connected((2,2),(3,2)).
% 2,3
connected((2,3),(1,3)).
connected((2,3),(2,2)).
connected((2,3),(2,3)).
connected((2,3),(2,4)).
connected((2,3),(3,3)).
% 2,4
connected((2,4),(1,4)).
connected((2,4),(2,3)).
connected((2,4),(2,4)).
connected((2,4),(2,5)).
connected((2,4),(3,4)).
% 2,5
connected((2,5),(1,5)).
connected((2,5),(2,4)).
connected((2,5),(2,5)).
connected((2,5),(3,5)).
% 3,1
connected((3,1),(2,1)).
connected((3,1),(3,1)).
connected((3,1),(4,1)).
connected((3,1),(3,2)).
% 3,2
connected((3,2),(3,1)).
connected((3,2),(3,2)).
connected((3,2),(3,3)).
connected((3,2),(2,2)).
connected((3,2),(4,2)).
% 3,3
connected((3,3),(3,2)).
connected((3,3),(3,3)).
connected((3,3),(3,4)).
connected((3,3),(2,3)).
connected((3,3),(4,3)).
% 3,4
connected((3,4),(3,3)).
connected((3,4),(3,4)).
connected((3,4),(3,5)).
connected((3,4),(2,4)).
connected((3,4),(4,4)).
% 3,5
connected((3,5),(3,4)).
connected((3,5),(3,5)).
connected((3,5),(4,5)).
connected((3,5),(2,5)).
% 4,1
connected((4,1),(4,1)).
connected((4,1),(3,1)).
connected((4,1),(5,1)).
connected((4,1),(4,2)).
% 4,2
connected((4,2),(4,1)).
connected((4,2),(4,2)).
connected((4,2),(4,3)).
connected((4,2),(3,2)).
connected((4,2),(5,2)).
% 4,3
connected((4,3),(4,2)).
connected((4,3),(4,3)).
connected((4,3),(4,4)).
connected((4,3),(3,3)).
connected((4,3),(5,3)).
% 4,4
connected((4,4),(4,3)).
connected((4,4),(4,4)).
connected((4,4),(4,5)).
connected((4,4),(3,4)).
connected((4,4),(5,4)).
% 4,5
connected((4,5),(4,4)).
connected((4,5),(4,5)).
connected((4,5),(3,5)).
connected((4,5),(5,5)).
% 5,1
connected((5,1),(4,1)).
connected((5,1),(5,1)).
connected((5,1),(5,2)).
% 5,2
connected((5,2),(5,1)).
connected((5,2),(5,2)).
connected((5,2),(5,3)).
connected((5,2),(4,2)).
% 5,3
connected((5,3),(5,2)).
connected((5,3),(5,3)).
connected((5,3),(5,4)).
connected((5,3),(4,3)).
% 5,4
connected((5,4),(5,3)).
connected((5,4),(5,4)).
connected((5,4),(5,5)).
connected((5,4),(4,4)).
% 5,5
connected((5,5),(5,4)).
connected((5,5),(5,5)).
connected((5,5),(4,5)).

%factory list
factories([]).

%Special piece location.
special(middle).

%pices gab
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

