:- use_module(library(http/http_open), [http_open/3]).
:- use_module(library(http/http_header)).      % support POST method
:- use_module(library(http/json), [atom_json_term/3]).
:- use_module(library(http/http_ssl_plugin)).  % support SSL
:- use_module(library(readutil), [read_stream_to_codes/2]).

http_get(Url, Response) :-
    http_open(Url, Stream, []),
    read_stream_to_codes(Stream, Response),
    close(Stream).

http_post(Url, Form, Response) :-
    http_open(Url, Stream, [post(form(Form))]),
    read_stream_to_codes(Stream, Codes),
    create_response(Response, Codes),
    close(Stream).

% convert a raw list of codes into a structured response according
% to the user's requests
create_response(codes(Codes), Codes).
create_response(json(Term), Codes) :-
    atom_codes(Atom, Codes),
    atom_json_term(Atom, Term, []).
