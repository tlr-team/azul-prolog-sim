:- use_module(library(random),[random_select/3]).
:- dynamic bag/1.


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

factory_gen(Size, Result) :-
    factory_gen(Size, [], Result).

factory_gen(0, Result, Result).

factory_gen(Size, Result, Acc) :-
    bag(L),
    random_select(S, L, R),
    retract(bag(L)),
    assert(bag(R)),
    NSize is Size - 1,
    factory_gen(NSize, [S|Result], Acc).


%factory_fill(0, Size, Factories) :-
%    assert(factories(Factories)).

%factory_fill(Number, Size, L) :-


