:- use_module(library(random),[random_select/3]).
:- dynamic bag/1.
:- dynamic factories/1.
:- dynamic special/1.

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

