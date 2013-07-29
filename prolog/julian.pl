/*
:- module(julian, [ form_time/2
                  , form_time/1
                  , day_of_week/2
                  , gregorian/3
                  , mjd/1
                  , nano/1
                  ]).
*/
:- use_module(library(clpfd)).
:- use_module(library(when), [when/2]).
:- use_module(library(dcg/basics), [float//1, integer//1, string//1]).

% This module represents times, dates and sets of those using
% terms of the form =|datetime(MJD, Nano)|=.  =MJD= is an
% integer representing the modified Julian day.  =Nano= is an
% integer representing the number of nanoseconds since midnight
% on that day.
%
% We indicate a date without time by leaving =Nano= as an
% unbound variable.  We indicate times without a date by
% leaving =MJD= unbound.  Arbitrary datetime sets are represented
% by using library(clpfd) constraints on =MJD= and =Nano=.
%
% This representation should make it very easy to implement
% datetime arithmetic predicates, although I've not yet done
% that below.

%%	gregorian(?Year, ?Month, ?Day) is det.
%
%	True if Year, Month and Day form a valid date in the Gregorian
%	calendar.  For example, one could iterate all leap years
%	since 1950 with this:
%
%	==
%	gregorian(Y, 2, 29), Y #> 1950, indomain(Y).
%	==
%
%	Because it just constrains Year, Month and Day to have the
%	proper relation one to another, one can bind as many or
%	as few of the arguments as desired.
gregorian(Y,M,D) :-
    Y in -4713..3267,
    M in 1..12,
    (   (D in 1..28)
    #\/ (M #\= 2 #/\ D in 29..30)
    #\/ (M in 1 \/ 3 \/ 5 \/ 7 \/ 8 \/ 10 \/ 12 #/\ D #= 31)
    #\/ (M #= 2 #/\ D #= 29 #/\ Y mod 400 #= 0)
    #\/ (M #= 2 #/\ D #= 29 #/\ Y mod 4 #= 0 #/\ Y mod 100 #\= 0)
    ).

%%	mjd(?MJD:integer) is semidet.
%
%	True if MJD is a valid modified Julian day number.
mjd(MJD) :-
    MJD in -2400328 .. 514671.

%%	nano(?Nano:integer) is semidet.
%
%	True if Nano is a valid number of nanoseconds since midnight.
nano(Nano) :-
    Nano in 0 .. 86_400_000_000_000.

%%	datetime(?Datetime, ?MJD, ?Nano) is semidet.
%
%   True if Datetime falls on modified Julian day MJD and occurs Nano
%   nanoseconds after midnight.
datetime(datetime(MJD, Nano), MJD, Nano) :-
    mjd(MJD),
    nano(Nano).

%%	datetime(?Datetime) is semidet.
%
%	True if Datetime is a date time term.
datetime(Dt) :-
    datetime(Dt, _, _).

%%	dow_number(?DayOfWeek:atom, ?Number:integer) is semidet.
%
%	True if Number is the ISO number for DayOfWeek.
%	0 is Monday, 6 is Sunday.
dow_number(monday,    0).
dow_number(tuesday,   1).
dow_number(wednesday, 2).
dow_number(thursday,  3).
dow_number(friday,    4).
dow_number(saturday,  5).
dow_number(sunday,    6).


%%	form_time(?Form, ?Datetime)
%
%	True if Datetime can be described by Form.  Form is
%	a sugary representation of a set of datetimes.  This
%	predicate is the workhorse for converting between
%	datetime values and other date representations. It's
%	also the workhorse for further constraining a datetime
%	value.
%
%	Here are some values for Form.
%
%		* `today` - the set of all seconds in the local day
%		* `now` - the current nanosecond
%		* `sunday` - the set of all Sundays in history
%		* `dow(tuesday)` - the set of all Tuesdays in history
%		* `dow([saturday,sunday])` - set of all weekends in history
%		* `unix(EpochSeconds)` - floating point seconds since the Unix
%		  epoch
%		* `[foo,bar]` - both `foo` and `bar` constraints apply
%		* `gregorian(Year,Month,Day)` - all seconds in a Gregorian
%		  date of the given form.  For example, `gregorian(_,3,_)`
%		  represents the set of all the months of March in history.
%       * `Year-Month-Day` - same as `gregorian(Year,Month,Day)`
%       * `Hours:Minutes:Seconds`
%       * `rfc3339(Text)` - the nanosecond indicated by the RFC 3339
%         date string.  Text can be atom or codes.
%
%	This predicate
%	is multifile because other modules might support different
%	calendars, different holiday schedules, etc.
:- multifile form_time/2.
form_time([], Dt) :-
    datetime(Dt).
form_time([H|T], Dt) :-
    form_time(H, Dt),
    form_time(T, Dt).
form_time(today, Dt) :-
    get_time(Now),
    stamp_date_time(Now, date(Year, Month, Day, _,_,_,_,_,_), local),
    form_time(gregorian(Year,Month,Day), Dt).
form_time(now, Dt) :-
    get_time(Now),
    form_time(unix(Now), Dt).
form_time(dow(Days), Dt) :-
    ground(Days),
    maplist(dow_number, Days, _),
    datetime(Dt, _, _),
    !,
    when(ground(Day), memberchk(Day,Days)),
    form_time(dow(Day), Dt).
form_time(dow(DayOfWeek), datetime(MJD, _)) :-
    (MJD+2) mod 7 #= DayNumber,
    when( (ground(DayOfWeek) ; ground(DayNumber))
        , dow_number(DayOfWeek, DayNumber)
        ),
    !.
form_time(Year-Month-Day, Dt) :-
    !,
    form_time(gregorian(Year,Month,Day), Dt).
form_time(gregorian(Year, Month, Day), Dt) :-
    gregorian(Year, Month, Day),
    datetime(Dt, MJD, _Nano),
    !,
    Y = 4716,
    V = 3,
    J = 1401,
    U = 5,
    M = 2,
    S = 153,
    N = 12,
    W = 2,
    R = 4,
    B = 274277,
    P = 1461,
    C = -38,
    F0 #= JD + J,
    F1 #= F0 + (((4 * JD + B)/146097) * 3)/4 + C,
    E #= R * F1 + V,
    G #= mod(E, P)/R,
    H #= U * G + W,
    Day #= (mod(H, S))/U + 1,
    Month #= mod(H/S + M, N) + 1,
    Year #= E/P - Y + (N + M - Month)/N,
    MJD #= JD - 2400001,
    ( ground(MJD) ->
        labeling([ff, up, bisect], [Year, Month, Day])
    ; ground(Year), ground(Month), ground(Day) ->
        labeling([leftmost, up, bisect], [MJD])
    ; true
    ).
form_time(Hours:Minutes:FloatSeconds, datetime(_, Nanos)) :-
    Second = 1_000_000_000,
    seconds_nanos(FloatSeconds, N),
    Hours   in 0 .. 23,
    Minutes in 0 .. 59,
    N       in 0 .. 59_999_999_999,
    Nanos #= Hours*60*60*Second + Minutes*60*Second + N.
form_time(unix(UnixEpochSeconds), datetime(Days, Nanos)) :-
    DayInNanos = 86_400_000_000_000,
    seconds_nanos(UnixEpochSeconds, N),
    ExtraDays #= N / DayInNanos,
    ExtraNanos #= N rem DayInNanos,

    % form_time([1970-01-01,00:00:00], datetime(40587,0))
    Days #= 40587 + ExtraDays,
    Nanos #= 0 + ExtraNanos.
form_time(rfc3339(Codes), datetime(Days, Nanos)) :-
    form_time([Y-Mon-D, H:Min:S], datetime(Days,Nanos)),
    once(phrase(rfc3339(Y,Mon,D,H,Min,S,_), Codes)).


%%	form_time(+Form) is semidet.
%
%	True if a date exists which satisfies Form.  For example,
%	"is May 1, 1979 a Tuesday?" would be
%
%	    form_time([1979-05-01,dow(tuesday)])
form_time(Form) :-
    form_time(Form, _).


%%	seconds_nanos(?Seconds:float, ?Nanos:integer) is semidet.
%
%   True if Seconds is a floating point representation of Nanos
%   nanoseconds.
seconds_nanos(Seconds, Nanos) :-
    when( (   ground(Seconds)
          ;   ground(Nanos)
          )
        , seconds_nanos_(Seconds, Nanos)
        ).
seconds_nanos_(Seconds, Nanos) :-
    number(Seconds),
    !,
    Nanos is floor(Seconds * 1_000_000_000).
seconds_nanos_(Seconds, Nanos) :-
    integer(Nanos),
    Seconds is Nanos / 1_000_000_000.


% padded_integer(W,N)//
%
% Describes a zero-padded integer N in a field exactly W characters
% wide.
padded_integer(W,N) -->
    { digit_len(N, DigitLen) },
    { lazy_plus(PadLen, DigitLen, W) },
    count(PadLen, 0'0),
    integer(N),
    !.


count(N0, X) -->
    { when( (ground(N);ground(N0)),succ(N, N0)) },
    [X],
    count(N, X).
count(0, _, L, L).


digit_len(N, Length) :-
    when(ground(N), digit_len_(N,Length)).
digit_len_(N, Length) :-
    ( N =:= 0 ->
        Length = 1
    ; % otherwise ->
        Length is floor(log10(N)) + 1
    ).


lazy_plus(X,Y,Z) :-
    when( ( (ground(X),ground(Y))
          ; (ground(X),ground(Z))
          ; (ground(Y),ground(Z))
          )
        , plus(X,Y,Z)
        ).


rfc3339(Y,Mon,D,H,Min,S,Zone) -->
    padded_integer(4, Y),
    "-",
    padded_integer(2, Mon),
    "-",
    padded_integer(2, D),
    "T",
    padded_integer(2, H),
    ":",
    padded_integer(2, Min),
    ":",
    ( float(S) ; integer(S) ),
    string(Zone),

    % and it must be a valid gregorian date
    { gregorian(Y,Mon,D) }.
