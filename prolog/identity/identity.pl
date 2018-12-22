:- module(identity, [
              identity_dispatch/2
          ]).
/** <module> identity - pack to manage user identities on the SWI-Prolog web framework.
 *
 *  this pack depends on OpenSSL1.1.0 or greater
 */

:- use_module(library(http/http_dispatch)).
:- use_module(library(http/http_path)).
:- use_module(library(url)).

:- multifile http:location/3.
:- dynamic   http:location/3.

http:location(login, root(login), [priority(-100)]).

:- use_module(library(identity/login_crypto)).
:- ensure_loaded(library(identity/login_page)).
:- use_module(library(identity/login_database), [user_has_role/2]).

:- meta_predicate identity_dispatch(1, +).
%!  identity_dispatch is det
%
%   A replacement dispatcher for the stock SWI-prolog dispatcher
%   http_dispatch.
%
%   Provides identity management services and then passes to the
%   stock http_dispatch.
%
%   **Identity** in this context is a username or email. The pack
%   provides services like login, logout, knowing if user is allowed
%   to visit this handler, etc.
%
%   Because a website might have arbitrarily complex rules about whether
%   you can see an arbitrary page, we only have two roles, guest (the
%   user is not logged in) and user (the user is logged in).
%
%   If a visitor requests a page to which they do not have access,
%   a 302 Found response will be generated. This 302 will redirect
%   to the abstract location login(.)
%
%   If the Referer header is present, and on the same domain, then
%   a message about 'you must be logged in to access that page' is
%   presented in the login page.
%
%   The login page itself is on a low priority handler. To make a custom
%   login page, use the provided DCGs in identity/login_page and
%   make your own.
%
%   usage:
%
%   http_server(identity_dispatch(http_dispatch), [.. server options
%   ...]).
%
identity_dispatch(OriginalDispatch, Request) :-
    request_pagerole(Request, PageRole),
    identity_dispatch_role(PageRole, OriginalDispatch, Request).

identity_dispatch_role(guest, OriginalDispatch, Request) :-
     call(OriginalDispatch, Request).
identity_dispatch_role(user, OriginalDispatch, Request) :-
    memberchk(cookie(Cookies), Request),
    memberchk(login=Token, Cookies),
    (   token_uname(Token, Uname)
    ->  call(OriginalDispatch, [identity(Uname) |Request])
    ;   redirect_to_login(Request)
    ),
    !.
identity_dispatch_role(user, _, Request) :-
    redirect_to_login(Request).
identity_dispatch_role(Role, OriginalDispatch, Request) :-
    \+ member(Role, [guest, user]),
    memberchk(cookie(Cookies), Request),
    memberchk(login=Token, Cookies),
    (   token_uname(Token, Uname),  % TODO make this go somewhere nice
        user_has_role(Uname, Role)
    ->  call(OriginalDispatch, [identity(Uname) |Request]),
        redirect_to_login(Request)
    ),
    !.
% another approach to providing the redirect location is to use a cookie
% in case folx complain about the search string

redirect_to_login(Request) :-
    memberchk(path(RedirPath), Request), % place to return to
    www_form_encode(RedirPath, URIEncRedirPath),
    http_absolute_location(login(.), Path, []),
    atomic_list_concat([Path, '?redirect=', URIEncRedirPath], URL),
    http_redirect(moved_temporary,
                  URL,
                  Request).

%!  request_pagerole(+Request:request, -PageRole:atom) is det
%
%   determine the pagerole of this page (contents of identity option)
%
request_pagerole(Request, PageRole) :-
    member(path(Path), Request),
    http_dispatch:find_handler(Path, _Handler, Options),
    (   member(identity(PageRole), Options)
    ;   PageRole = guest
    ).














