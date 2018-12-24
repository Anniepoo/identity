:- module(logout, []).
/** <module> implement the logout page
 *
 */

:- use_module(library(http/http_dispatch)).
:- use_module(library(http/http_client)).

:-http_handler(login(logout), logout_handler, [id(logout), identity(guest), priority(-100)]).


logout_handler(Request) :-
    member(method(post), Request),
    http_read_data(Request, Data, []),
    (   member(referer=Redirect, Data)
    ;
        http_location_by_id(home, Redirect)
    ),
    format('Status: 302 Found~n'),
    format('Location: ~w~n', [Redirect]),
    format('Set-Cookie: login=""; Path=/; Max-Age=-1~n', []),
    format('Content-type: text/plain~n~n').
logout_handler(_Request) :-
    http_location_by_id(home, Redirect),
    format('Status: 302 Found~n'),
    format('Location: ~w~n', [Redirect]),
    format('Set-Cookie: login=""; Path=/; Max-Age=-1~n', []),
    format('Content-type: text/plain~n~n').
