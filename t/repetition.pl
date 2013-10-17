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
'final weekday' :-
    phrase(repetition(X), "final weekday"),
    X == nth(-1, weekday).


'each day' :-
    phrase(repetition(X), "each day"),
    X == true.


'single month' :-
    phrase(repetition(X), "August"),
    X == month(august).
'months list' :-
    phrase(repetition(X), "January, April, july and October"),
    X == month([january,april,july,october]).


'day of week in month' :-
    phrase(repetition(X), "second friday in May"),
    X == [month(may), nth(2,dow(friday))].


'list of refinements' :-
    phrase(repetition(X), "first Monday of April in even years"),
    X = [gregorian(_, _, _), month(april), nth(1, dow(monday))].


'month name and year' :-
    phrase(repetition(X), "August 2013"),
    X = gregorian(2013,8,_).


'second and fourth Sunday' :-
    phrase(repetition(X), "second and fourth sunday"),
    X == nth([2,4], dow(sunday)).


'month name and day number' :-
    phrase(repetition(X), "February 1st"),
    X = gregorian(Y,M,D),
    var(Y),
    M == 2,
    D == 1.
