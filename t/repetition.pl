% vim: ft=prolog
:- [prolog/tomorrow].
:- use_module(library(tap)).

% simple day of the week names
english_constraints("Monday", [dow(monday)]).
english_constraints("monday", [dow(monday)]).


weekday :-
    english_constraints("weekday", X),
    forall( member(Day,[monday,tuesday,wednesday,thursday,friday])
          , X = [dow(Day)]
          ),
    X \= [dow(saturday)],
    X \= [dow(sunday)].
