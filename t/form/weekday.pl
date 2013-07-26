% vim: ft=prolog
:- [prolog/julian].
:- use_module(library(tap)).


'weekday(thursday), forward' :-
    form_time(gregorian(1970,1,1), Dt),
    form_time(weekday(thursday), Dt).
'weekday(thursday), backward' :-
    form_time(gregorian(1970,1,1), Dt),
    form_time(weekday(Weekday), Dt),
    Weekday = thursday.
'weekday(thursday), compound' :-
    form_time([weekday(Weekday), gregorian(1970,1,1)], _),
    Weekday = thursday.
