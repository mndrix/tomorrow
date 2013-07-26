:- use_module(library(func)).
:- use_module(library(readutil), [read_line_to_codes/2]).
:- use_module(library(uri_qq)).
:- use_module(library(dcg/basics), [float//1, integer//1, string//1]).


:- [http, julian].

% tokens( refresh_token:atom
%       , access_token:atom
%       , expiry:unix_epoch_seconds
%       )
refresh_token(tokens(X,_,_), X).
access_token(tokens(_,X,_), X).
expiry(tokens(_,_,X), X).
is_access_token_expired(Tokens) :-
    expiry(Tokens, Expiry),
    get_time(Now),
    Now > Expiry.


% tasklist(id:atom, title:atom)
:- discontiguous id/2, title/2.
id(tasklist(X,_), X).
title(tasklist(_,X), X).


% task(id:atom, title:atom, notes:atom, due:atom, status:atom)
%
% notes='' means the field is missing.
% due='' means the field is missing.
% status=needsAction | completed.
id(task(X,_,_,_,_), X).
title(task(_,X,_,_,_), X).
notes(task(_,_,X,_,_), X).
due(task(_,_,_,X,_), X).
status(task(_,_,_,_,X), X).

% setters
due( task(Id,Title,Notes,DueOld,Status)
   , DueOld
   , DueNew
   , task(Id,Title,Notes,DueNew,Status)
   ).


main(_) :-
    get_access_token(AccessToken),

    % find Future list
    tasklist(AccessToken, Future),
    title(Future, 'Future'),
    format('future id = ~w~n', id $ Future),

    % find Today list
    tasklist(AccessToken, Today),
    title(Today, 'Today'),
    format('today id = ~w~n', id $ Today),
    nl,

    % tasks in the Future list
    task(AccessToken, Future, Template),
    template_applicable(Template, Status),

    specialize_template(Template, Task),
    insert_task(AccessToken, Today, Task, Inserted),
    write_quoted(Inserted),
    nl,

    % clean up templates, if they need it
    ( Status = delete ->
        delete_task(AccessToken, Future, Template),
        writeln('deleted template that served its purpose')
    ; % otherwise ->
        true
    ),

    nl,
    fail.


% TODO support all insertable fields of a task
insert_task(AccessToken, TaskList, Task, Inserted) :-
    TaskListId = _, % hack around quasiquote singleton warning
    AccessToken = _,

    % build JSON structure. intentionally omit due date
    Request = json([ title = title $ Task
                   , notes = notes $ Task
                   ]),

    __uri_qq_base = 'https://www.googleapis.com/tasks/v1/',
    TaskListId = id $ TaskList,
    http_post( {|uri||lists/$TaskListId/tasks?access_token=$AccessToken|}
             , json(Request)
             , json(Response)
             ),
    json_task(Response, Inserted).


delete_task(AccessToken, TaskList, Task) :-
    TaskListId = _, % hack around quasiquote singleton warning
    TaskId = _,
    AccessToken = _,

    __uri_qq_base = 'https://www.googleapis.com/tasks/v1/',
    TaskListId = id $ TaskList,
    TaskId = id $ Task,
    Uri = {|uri||lists/$TaskListId/tasks/$TaskId?access_token=$AccessToken|},
    http_delete(Uri,_).



specialize_template(Template, Task) :-
    due(Template, _, '', Task).


% template_applicable(+Template:task, -Status)
template_applicable(Task, delete) :-
    % bare tasks always belong in Today
    due(Task, ''),
    repeats(Task, '').
template_applicable(Task, delete) :-
    % task is due today
    due(Task, Due),
    date_is(Due, today).
template_applicable(Task, retain) :-
    % task schedule falls on today
    due(Task, ''),
    repeats(Task, Constraints),
    date_is(_, [today|Constraints]).

% make all these calculations symmetric
date_is(mjd(Days, Nanos), unix(UnixEpochSeconds)) :-
    U = rationalize(UnixEpochSeconds),
    DaysBeforeUnixEpoch   = (24405875 rdiv 10),
    OffsetBetweenJDandMJD = (24000005 rdiv 10),

    % TODO nanosecond calculation is wrong
    % TODO use clp(fd) to make this work in both directions
    MJD is (U rdiv 86400) + DaysBeforeUnixEpoch - OffsetBetweenJDandMJD,
    Days is floor(MJD),
    Nanos is floor((MJD - Days) * 86_400 * 1_000_000_000).
date_is(mjd(Days, Nanos), rfc3339(Text)) :-
    text_codes(Text, Codes),
    phrase(rfc3339(Year,Month,Day,_,_,_,_), Codes),
    % TODO make date_name/2 clauses into date_is/2 clauses
    date_name(datetime(Days,Nanos), gregorian(Year, Month, Day)).
date_is(mjd(Day, _), today) :-
    get_time(Now),
    date_is(mjd(Day,_), unix(Now)).


text_codes(Text, Codes) :-
    atom(Text),
    !,
    atom_codes(Text, Codes).
text_codes(Text, Text) :-
    is_list(Text).


% padded_integer(W,N)//
%
% Describes a zero-padded integer N in a field exactly W characters
% wide.
padded_integer(W, N, A, B) :-
    integer(W),
    integer(N),
    !,
    number_codes(N, Numbers),
    length(Numbers, NumbersLen),
    plus(ZerosLen, NumbersLen, W),
    length(Zeros, ZerosLen),
    maplist(=(0'0), Zeros),
    format(codes(A,B), '~s~s', [Zeros, Numbers]).
padded_integer(W0,N) -->
    "0",
    !,
    padded_integer(W,N),
    { succ(W, W0) }.
padded_integer(W,N) -->
    integer(N),
    { number_codes(N, Codes) },
    { length(Codes, W) }.
padded_integer(0, _) -->
    [].


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
    float(S),
    string(Zone),

    % and it must be a valid gregorian date
    { gregorian(Y,Mon,D) }.


% TODO imlement this
repeats(_Task, _Constraints) :-
    fail.


% tasklist(+AccessToken, -TaskList) is nondet.
%
% True if TaskList is a current list belonging to user represented
% by AcessToken.
tasklist(AccessToken, TaskList) :-
    % cached tasklist values are outdated; rebuild cache.

    % TODO factor out predicate for building tasks URIs
    % TODO factor out predicate for making tasks API requests

    AccessToken = _, % hack around quasiquote singleton warning
    __uri_qq_base = 'https://www.googleapis.com/tasks/v1/',
    http_get( {|uri||users/@me/lists?access_token=$AccessToken|}
            , json(JSON)
            ),

    json_get(JSON, items, Items),
    member(Item, Items),
    json_get(Item, id, Id),
    json_get(Item, title, Title),
    TaskList = tasklist(Id, Title).


%% task(+AccessToken, +TaskList, -Task) is nondet.
%
%  True if Task is a child task of TaskList for the user represented by
%  AccessToken.
task(AccessToken, TaskList, Task) :-
    AccessToken = _, % hack around quasiquote singleton warnings
    TaskListId = _,

    __uri_qq_base = 'https://www.googleapis.com/tasks/v1/',
    id(TaskList, TaskListId),
    http_get( {|uri||lists/$TaskListId/tasks?access_token=$AccessToken|}
            , json(JSON)
            ),

    json_get(JSON, items, Items),
    member(Item, Items),
    json_task(Item, Task).


%%	json_task(+JSON, -Task)
%
%	True if Task represents the Google Task encoded in JSON.
json_task(Item, Task) :-
    json_get(Item, id, Id),
    json_get(Item, title, Title),
    ( json_get(Item, notes, Notes) -> true; Notes='' ),
    ( json_get(Item, due, RFC3339) ->
        date_is(Due, rfc3339(RFC3339))
    ; % otherwise ->
        Due=''
    ),
    json_get(Item, status, Status),
    Task = task(Id, Title, Notes, Due, Status).


% TODO factor out each OAuth step to a library
get_access_token(AccessToken) :-
    % look for a cached token
    tokens_cache(read, Tokens0),
    !,
    freshen_access_token(Tokens0, Tokens),
    access_token(Tokens, AccessToken).
get_access_token(AccessToken) :-
    % use OAuth flow to acquire an access code
    Query = _, % hack around quasiquote singleton warning
    Query = [ response_type = code
            , client_id     = '103740898794.apps.googleusercontent.com'
            , redirect_uri  = 'urn:ietf:wg:oauth:2.0:oob'
            , scope         = 'https://www.googleapis.com/auth/tasks'
            , access_type   = offline
            ],
    open_browser({|uri||https://accounts.google.com/o/oauth2/auth?$Query|}),
    write('Paste code here: '),
    current_input(In),
    read_line_to_codes(In, Code),

    % POST access code to Google to receive full tokens
    Form = [ code          = Code
           , client_id     = '103740898794.apps.googleusercontent.com'
           , client_secret = 'KAisT0QCdtJ8exXtQ48tJQeM'
           , redirect_uri  = 'urn:ietf:wg:oauth:2.0:oob'
           , grant_type    = authorization_code
           ],
    http_post( {|uri||https://accounts.google.com/o/oauth2/token|}
             , form(Form)
             , json(JSON)
             ),
    extract_tokens(JSON, Tokens),
    tokens_cache(write, Tokens),
    access_token(Tokens, AccessToken).


tokens_cache(read, Token) :-
    % read contents from cache
    read_file_to_terms('oauth.token', [Token], [file_errors(fail)]).
tokens_cache(write, Token) :-
    % update cache contents
    tell('oauth.token'),
        write_quoted(Token),
        write('.'),
        nl,
    told.


freshen_access_token(Tokens0, Tokens) :-
    % can we just keep using the current access token?

    \+ is_access_token_expired(Tokens0),
    !,
    Tokens = Tokens0.
freshen_access_token(Tokens0, Tokens) :-
    % access token has expired so we need a new one

    % use refresh_token to get a new access token
    Form = [ refresh_token = refresh_token $ Tokens0
           , client_id     = '103740898794.apps.googleusercontent.com'
           , client_secret = 'KAisT0QCdtJ8exXtQ48tJQeM'
           , grant_type    = refresh_token
           ],
    http_post( {|uri||https://accounts.google.com/o/oauth2/token|}
             , form(Form)
             , json(JSON)
             ),
    extract_tokens(JSON, Tokens),

    % old and new have the same refresh token
    refresh_token(Tokens, refresh_token $ Tokens0),

    % write tokens to cache
    tokens_cache(write, Tokens).


% TODO factor out to library?
json_get(json(Object), Key, Value) :-
    memberchk(Key=Value, Object).


extract_tokens(JSON, tokens(Refresh, Access, Expiry)) :-
    json_get(JSON, access_token, Access),
    json_get(JSON, expires_in, Expires),
    ignore(json_get(JSON, refresh_token, Refresh)), % optional field

    % calculate expiry time with a small safety factor
    get_time(Now),
    Expiry is floor(Now) + Expires - 10.


write_quoted(Term) :-
    write_term(Term, [quoted(true)]).

% TODO factor out to a library
% open_browser(+Uri:atom) is det.
%
% Opens Uri in the user's default web browser.
open_browser(Uri) :-
    operating_system(Os),
    open_browser(Os, Uri).

open_browser(osx, Uri) :-
    shell('open "~w"' $ Uri).

% TODO factor out to a library
% TODO implement in terms of Prolog flag which provides this information
% operating_system(-OS:atom) is det.
%
% True if this code is running on an operating system described by OS.
% Possible values are:
%
%   * osx - Mac OS X
operating_system(osx).


