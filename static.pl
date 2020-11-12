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

%pieces bag
default_bag([azul, azul, azul, azul, azul,
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

default_table([
        (1, [], none),
        (2, [], none),
        (3, [], none),
        (4, [], none),
        (5, [], none)
    ]).

default_board([
        [
            (1,1,azul),
            (1,2,naranja),
            (1,3,rojo),
            (1,4,negro),
            (1,5,blanco),

            (2,1,blanco),
            (2,2,azul),
            (2,3,naranja),
            (2,4,rojo),
            (2,5,negro),

            (3,1,negro),
            (3,2,blanco),
            (3,3,azul),
            (3,4,naranja),
            (3,5,rojo),

            (4,1,rojo),
            (4,2,negro),
            (4,3,blanco),
            (4,4,azul),
            (4,5,naranja),

            (5,1,naranja),
            (5,2,rojo),
            (5,3,negro),
            (5,4,blanco),
            (5,5,azul)
        ]).


% amount of pieces on the players floor
floor(0, 0), !.
floor(1, -1), !.
floor(2, -2), !.
floor(3, -4), !.
floor(4, -6), !.
floor(5, -8), !.
floor(6, -11), !.
floor(7, -14), !.
% careful with this case
floor(_, -14).


factories_number(9).
pieces_per_factory(4).

% list of all position combinations of the colours in the board
complete_colours([
    [(1,1),(2,2),(3,3),(4,4),(5,5)], %blue colour
    [(1,2),(2,3),(3,4),(4,5),(5,1)], %orange colour
    [(1,3),(2,4),(3,5),(4,1),(5,2)], %red colour
    [(1,4),(2,5),(3,1),(4,2),(5,3)], %Black colour
    [(1,5),(2,1),(3,2),(4,3),(5,4)]]). %white colour

% number row list
row([1,2,3,4,5]).

% color list
colors([azul, naranja, rojo, negro, blanco]).