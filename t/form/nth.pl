% vim: ft=prolog
:- [prolog/julian].
:- use_module(library(tap)).

'fifth monday, July 2013' :-
    form_time([2013-07-Day, nth(5, dow(monday))]),
    Day =:= 29.

'second weekday, Februray 2002' :-
    form_time([2002-02-Day, nth(2, weekday)]),
    Day =:= 4.

'final weekday, August 2013' :-
    form_time([2013-08-Day, nth(-1, weekday)]),
    Day =:= 30.
