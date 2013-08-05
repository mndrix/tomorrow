% vim: ft=prolog
:- [prolog/julian].
:- use_module(library(tap)).

'lookup: works like nth0' :-
    Xs = [a,b,c,d,e],
    circular_nth0(0, Xs, A),
    circular_nth0(1, Xs, B),
    circular_nth0(2, Xs, C),
    circular_nth0(3, Xs, D),
    circular_nth0(4, Xs, E),
    \+ circular_nth0(5, Xs, _),
    A == a,
    B == b,
    C == c,
    D == d,
    E == e.

'lookup: negative indexes' :-
    Xs = [a,b,c,d,e],
    circular_nth0(-5, Xs, A),
    circular_nth0(-4, Xs, B),
    circular_nth0(-3, Xs, C),
    circular_nth0(-2, Xs, D),
    circular_nth0(-1, Xs, E),
    \+ circular_nth0(-6, Xs, _),
    A == a,
    B == b,
    C == c,
    D == d,
    E == e.
