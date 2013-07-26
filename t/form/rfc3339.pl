% vim: ft=prolog
:- [prolog/julian].
:- use_module(library(tap)).

parsing :-
    form_time(rfc3339("2013-07-26T17:58:08.000Z"), Dt),
    form_time(2013-07-26, Dt),
    form_time(17:58:08.000, Dt).

formatting :-
    form_time(unix(1374876170), Dt), form_time(rfc3339(T), Dt),
    T = "2013-07-26T22:02:50".

'formatting with fractional seconds' :-
    form_time(unix(1374876170.2), Dt), form_time(rfc3339(T), Dt),
    T = "2013-07-26T22:02:50.2".
