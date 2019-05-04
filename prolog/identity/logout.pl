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
        [ referer(Return, [default(Redirect)])   % TODO check this
        ]),
    http_session_retractall(user(_User)),
    ignore((
        http_in_session(SessionID),
        http_close_session(SessionID)
    )),
    format('Set-Cookie: login=0; Expires=Thu, 01 Jan 1970 00:00:00 GMT; SameSite=Lax; Version=1; HttpOnly; Path=/\r\n'),
    http_redirect(see_other, Return, Request).
% TODO expire remember_me cookie on logout


% TODO at the moment we destroy sessions on logout.
% users running auto created sessions should
