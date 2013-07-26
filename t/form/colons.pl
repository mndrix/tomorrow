% vim: ft=prolog
:- [prolog/julian].
:- use_module(library(tap)).

midnight :-
    form_time(00:00:00, Dt),
    datetime(Dt, _, 0).
