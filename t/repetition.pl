% vim: ft=prolog
:- [prolog/tomorrow].
:- use_module(library(tap)).

% simple day of the week names
phrase(repetition(dow(monday)),"Monday").
phrase(repetition(dow(thursday)),"thursday").


% an empty repitition is no repition at all
empty(fail) :-
    phrase(repetition(_),"").


'jibberish after day of week'(fail) :-
    phrase(repetition(dow(friday)),"friday is fun").


weekday :-
    phrase(repetition(X), "weekday"),
    X == weekday.


'days of the week list' :-
    phrase(repetition(X), "monday, Tuesday or thursday"),
    X == dow([monday,tuesday,thursday]).


'second tuesday' :-
    phrase(repetition(X), "second tuesday"),
    X == nth(2, dow(tuesday)).

'each day' :-
    phrase(repetition(X), "each day"),
    X == true.


'single month' :-
    phrase(repetition(X), "August"),
    X == month(august).
'months list' :-
    phrase(repetition(X), "January, April, july and October"),
    X == month([january,april,july,october]).
