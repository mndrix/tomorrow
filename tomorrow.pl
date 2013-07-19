:- use_module(library(func)).
:- use_module(library(readutil), [read_line_to_codes/2]).
:- use_module(library(uri_qq)).


:- [http].

% tokens(RefreshToken:atom, AccessToken:atom, Expiry:unix_epoch_seconds)
refresh_token(tokens(X,_,_), X).
access_token(tokens(_,X,_), X).
expiry(tokens(_,_,X), X).
is_access_token_expired(Tokens) :-
    expiry(Tokens, Expiry),
    get_time(Now),
    Now > Expiry.


main(_) :-
    get_access_token(AccessToken),
    writeln(AccessToken).


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
             , Form
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
    Form = [ refresh_token = '1/Rd_2ExZKaxX7DomVHfGJ6K7vuHn7MkGbZX8HG2lNagk'
           , client_id     = '103740898794.apps.googleusercontent.com'
           , client_secret = 'KAisT0QCdtJ8exXtQ48tJQeM'
           , grant_type    = refresh_token
           ],
    http_post( {|uri||https://accounts.google.com/o/oauth2/token|}
             , Form
             , json(JSON)
             ),
    extract_tokens(JSON, Tokens),

    % old and new have the same refresh token
    refresh_token(Tokens, refresh_token $ Tokens0),

    % write tokens to cache
    tokens_cache(write, Tokens).


extract_tokens(json(Object), tokens(Refresh, Access, Expiry)) :-
    memberchk(access_token=Access, Object),
    memberchk(expires_in=Expires, Object),
    ignore(memberchk(refresh_token=Refresh, Object)),  % optional field

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


