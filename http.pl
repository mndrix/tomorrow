:- use_module(library(http/http_open), [http_open/3]).
:- use_module(library(http/http_header)).      % support POST method
:- use_module(library(http/json), [atom_json_term/3]).
:- use_module(library(http/http_ssl_plugin)).  % support SSL
:- use_module(library(readutil), [read_stream_to_codes/2]).

:- dynamic http_get_cached/3.  % Url:atom, Expiry:epoch_seconds, Response

http_get(Url, Response) :-
    % can we use a cached response?

    http_get_cached(Url, Expiry, Response0),
    get_time(Now),
    Now < Expiry,
    !,
    Response = Response0.
http_get(Url, Response) :-
    % nope, we must fetch a response over the network

    retractall(http_get_cached(Url, _, _)),  % remove stale cache entries
    http_open(Url, Stream, []),
    read_stream_to_codes(Stream, Codes),
    create_response(Response, Codes),
    close(Stream),

    % cache results for one minute
    get_time(Now),
    Expiry is Now + 60,
    assertz(http_get_cached(Url, Expiry, Response)).

http_post(Url, json(JSON), Response) :-
    % POSTing a JSON body
    !,
    atom_json_term(Codes, JSON, [as(codes)]),
    http_post(Url, codes('application/json', Codes), Response).
http_post(Url, Data, Response) :-
    http_open(Url, Stream, [post(Data)]),
    read_stream_to_codes(Stream, Codes),
    create_response(Response, Codes),
    close(Stream).

% convert a raw list of codes into a structured response according
% to the user's requests
create_response(codes(Codes), Codes).
create_response(json(Term), Codes) :-
    atom_codes(Atom, Codes),
    atom_json_term(Atom, Term, []).
