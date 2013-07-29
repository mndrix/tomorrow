% vim: ft=prolog
:- [prolog/tomorrow].
:- use_module(library(tap)).

% simple day of the week names
phrase(repetition(dow(monday)),"Monday").
phrase(repetition(dow(thursday)),"thursday").


weekday :-
    phrase(repetition(X), "weekday"),
    forall( member(Day,[monday,tuesday,wednesday,thursday,friday])
          , X = dow(Day)
          ),
    X \= dow(saturday),
    X \= dow(sunday).
