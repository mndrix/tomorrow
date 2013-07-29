% vim: ft=prolog
:- [prolog/tomorrow].
:- use_module(library(tap)).

% simple day of the week names
phrase(repetition(dow(monday)),"Monday").
phrase(repetition(dow(thursday)),"thursday").


'jibberish after day of week'(fail) :-
    phrase(repetition(dow(friday)),"friday is fun").


weekday :-
    phrase(repetition(X), "weekday"),
    forall( member(Day,[monday,tuesday,wednesday,thursday,friday])
          , X = dow(Day)
          ),
    forall( member(Day,[saturday, sunday])
          , X \= dow(Day)
          ).
