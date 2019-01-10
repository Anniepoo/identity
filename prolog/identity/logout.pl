:- module(logout, []).
/** <module> implement the logout page
 *
 */

:- use_module(library(http/http_dispatch)).
:- use_module(library(http/http_client)).
:- use_module(library(http/http_session)).
:- use_module(library(http/http_parameters)).

:-http_handler(login(logout), logout_handler, [id(logout), identity(guest), priority(-100)]).

logout_handler(Request) :-
    http_location_by_id(home, Redirect),
    http_parameters(
        Request,
        [ referer(Return, [Redirect])   % TODO check this
        ]),
    http_session_retractall(user(_User)),
    http_redirect(see_other, Return, Request).
% TODO expire remmember_me cookie on logout
