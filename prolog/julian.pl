/*
:- module(julian, [ form_time/2
                  , day_of_week/2
                  , gregorian/3
                  , mjd/1
                  , nano/1
                  ]).
*/
:- use_module(library(clpfd)).

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

%%	mjd(?MJD:integer) is det.
%
%	True if MJD is a valid modified Julian day number.
mjd(MJD) :-
    MJD in -2400328 .. 514671.

%%	nano(?Nano:integer) is det.
%
%	True if Nano is a valid number of nanoseconds since midnight.
nano(Nano) :-
    Nano in 0 .. 86_400_000_000_000.

%%	datetime(?Datetime, ?MJD, ?Nano) is det.
%
%   True if Datetime falls on modified Julian day MJD and occurs Nano
%   nanoseconds after midnight.
datetime(datetime(MJD, Nano), MJD, Nano) :-
    mjd(MJD),
    nano(Nano).


%%	weekday_number(?Weekday:atom, ?Number:integer) is semidet.
%
%	True if Number is the ISO number for Weekday.
%	0 is Monday, 6 is Sunday.
weekday_number(monday,    0).
weekday_number(tuesday,   1).
weekday_number(wednesday, 2).
weekday_number(thursday,  3).
weekday_number(friday,    4).
weekday_number(saturday,  5).
weekday_number(sunday,    6).


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
%		* `weekday(tuesday)` - the set of all Tuesdays in history
%		* `unix(EpochSeconds)` - floating point seconds since the Unix
%		  epoch
%		* `[foo,bar]` - both `foo` and `bar` constraints apply
%		* `gregorian(Year,Month,Day)` - all seconds in a Gregorian
%		  date of the given form.  For example, `gregorian(_,3,_)`
%		  represents the set of all the months of March in history.
%       * `rfc3339(Text)` - the nanosecond indicated by the RFC 3339
%         date string.  Text can be atom or codes.
%
%	As a demonstration, `july_fourth` constrains Datetime to
%	the Fourth of July holiday in 1776 or later.
%
%	This predicate
%	is multifile because other modules might support different
%	calendars, different holiday schedules, etc.
:- multifile form_time/2.
form_time(today, Dt) :-
    get_time(Now),
    stamp_date_time(Now, date(Year, Month, Day, _,_,_,_,_,_), local),
    form_time(gregorian(Year,Month,Day), Dt).
form_time(now, Dt) :-
    get_time(Now),
    form_time(unix(Now), Dt).
form_time(weekday(Weekday), datetime(MJD, _)) :-
    (MJD+2) mod 7 #= DayNumber,
    weekday_number(Weekday, DayNumber).
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
form_time(unix(UnixEpochSeconds), datetime(Days, Nanos)) :-
    U = rationalize(UnixEpochSeconds),
    DaysBeforeUnixEpoch   = (24405875 rdiv 10),
    OffsetBetweenJDandMJD = (24000005 rdiv 10),

    % TODO nanosecond calculation is wrong
    % TODO use clp(fd) to make this work in both directions
    MJD is (U rdiv 86400) + DaysBeforeUnixEpoch - OffsetBetweenJDandMJD,
    Days is floor(MJD),
    Nanos is floor((MJD - Days) * 86_400 * 1_000_000_000).
form_time(rfc3339(Text), datetime(Days, Nanos)) :-
    text_codes(Text, Codes),
    phrase(rfc3339(Year,Month,Day,_,_,_,_), Codes),
    form_time(gregorian(Year, Month, Day), datetime(Days,Nanos)).
form_time(Weekday, Dt) :-     % bare day of week constraints
    weekday_number(Weekday, _),
    !,
    form_time(weekday(Weekday), Dt).


:- begin_tests(acceptable_gregorian_dates).
test(march_16_2013) :-
    gregorian(2013,3,16).

% leap year rules
test(1900, [fail]) :-
    gregorian(1900,2,29).
test(2000) :-
    gregorian(2012,2,29).
test(2012) :-
    gregorian(2012,2,29).
test(2013, [fail]) :-
    gregorian(2013,2,29).
test(2014, [fail]) :-
    gregorian(2014,2,29).
test(2016) :-
    gregorian(2016,2,29).
:- end_tests(acceptable_gregorian_dates).


:- begin_tests(gregorian_conversion).
test(march_16_2013) :-
    form_time(gregorian(2013,3,16), datetime(56367, _)).
test(unix_epoch) :-
    form_time(gregorian(1970,1,1), datetime(40587, _)).
:- end_tests(gregorian_conversion).
