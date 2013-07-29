:- use_module(library(func)).
:- use_module(library(readutil), [read_line_to_codes/2]).
:- use_module(library(uri_qq)).
:- use_module(library(dcg/basics), [string_without//2]).


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
notes( task(Id,Title,NotesOld,Due,Status)
     , NotesOld
     , NotesNew
     , task(Id,Title,NotesNew,Due,Status)
     ).
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
    % TODO for debugging
    %format('would have inserted to Today: ~w~n', [Task]),
    %fail,
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
    % remove due date
    due(Template, _, '', Task0),

    % remove repetition rule from notes
    repeats(Task0, _Rule, Lines),
    maplist(atom_codes, Atoms, Lines),
    atomic_list_concat(Atoms, '\n', Notes),
    notes(Task0, _, Notes, Task).


%%  template_applicable(+Template:task, -Status:atom)
%
%   True if Template represents a task template that's applicable to the
%   current day. Status is unified with a value indicating what should
%   be done with the Template.  It's one of: `delete` or `retain`.
template_applicable(Task, delete) :-
    % bare tasks always belong in Today
    due(Task, ''),
    repeats(Task, "", _).
template_applicable(Task, delete) :-
    % task is due today
    due(Task, Due),
    form_time(today, Due).
template_applicable(Task, retain) :-
    % task schedule falls on today
    due(Task, ''),
    repeats(Task, English, _),
    English \= "",
    ( phrase(repetition(Constraints), English) ->
        format('Recognized repetition: ~s~n', [English]),
        form_time([today|Constraints]),
        format('    => ~w~n', [Constraints])
    ; % otherwise ->
        format('Unknown repetition: ~s~n', [English]),
        fail
    ).


repeats(Task, Rule, Others) :-
    phrase(repeat_on(Rule0, Others0), atom_codes $ notes $ Task, _),
    !,
    Rule = Rule0,
    Others = Others0.
repeats(_, "", []).

repeat_on(Rule, [], [], []) :-
    ( var(Rule) ->
        Rule = ""
    ; % otherwise ->
        true
    ).
repeat_on(Rule, Others) -->
    "Repeat on ",
    string_without("\n", RestOfLine),
    !,
    ( "\n" ; end_of_content ),
    { Rule = RestOfLine },
    repeat_on(Rule, Others).
repeat_on(Rule, [Line|Others]) -->
    string_without("\n", Line),
    ( "\n" ; end_of_content ),
    repeat_on(Rule, Others).


end_of_content([],[]).


% TODO make string//1 an argument like Separator
% TODO so that I can call phrase(split(comma, day_of_week, Parts), Xs)
split(Separator, [Part|Parts]) -->
    string(Part),
    Separator,
    split(Separator, Parts).
split(_, [Part], Part, []).


comma --> " and ".
comma --> ", ".
comma --> ",".


% True if Day is an atom representing the day of week named in Codes
codes_dow(Codes, Day) :-
    atom_codes(Atom, Codes),
    downcase_atom(Atom, Day),
    dow_number(Day, _).


repetition(dow(Day)) -->
    "weekday",
    !,
    { Weekdays = [monday,tuesday,wednesday,thursday,friday] },
    { when(ground(Day), memberchk(Day, Weekdays)) }.
repetition(dow(Day)) -->
    string(Word),
    end_of_content,
    { codes_dow(Word, Day) },
    !.


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
        form_time(rfc3339(atom_codes $ RFC3339), Due)
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


