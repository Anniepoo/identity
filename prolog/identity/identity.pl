:- module(identity, [
              current_user/1
          ]).
/** <module> identity - pack to manage user identities on the SWI-Prolog web framework.
 *
 *  this pack depends on OpenSSL1.1.0 or greater
 */

:- use_module(library(http/thread_httpd)).
:- use_module(library(http/http_dispatch)).
:- use_module(library(http/http_path)).
:- use_module(library(url)).
:- use_module(library(http/http_parameters)).
:- use_module(library(http/http_session)).
:- use_module(library(http/html_write)).

:- multifile http:location/3.
:- dynamic   http:location/3.

http:location(login, root(login), [priority(-100)]).

:- use_module(library(identity/login_crypto)).
:- ensure_loaded(library(identity/login_page)).
:- ensure_loaded(library(identity/logout)).
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
%   to the abstract location login(.) if they need to log in, and
%   to an error page if they're logged in but don't have the right role.
%   TODO and what if they're not logged in but it's not a user page?
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
    ->  b_setval(current_user, Uname),
        call(OriginalDispatch, [identity(Uname) |Request])
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
    ->  b_setval(current_user, Uname),
        call(OriginalDispatch, [identity(Uname) |Request])
    ;
        redirect_to_login(Request)
    ),
    !.
% another approach to providing the redirect location is to use a cookie
% in case folx complain about the search string

redirect_to_login(Request) :-
    memberchk(path(RedirPath), Request), % place to return to
    www_form_encode(RedirPath, URIEncRedirPath),
    http_location_by_id(login_form, Path),
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


current_user(UName) :-
    http_session_data(user(UName)).
current_user(guest).

		 /*******************************
		 *            USER DATA		*
		 *******************************/


%!  valid_passwd(+User, +Passwd) is semidet.
%
%   True if Passwd is the correct password for User.
% TODO subsumed by user/2
%
valid_passwd(User, Passwd) :-
    user(User, password(Passwd)).

%!  user(?User, ?Property) is nondet.
%
%   True when Property is a property of user. In a real application this
%   should of course be  a  proper   persistent  database  and passwords
%   should be properly hashed.

user(jan, password(geheim)).
user(jan, role(trusted)).
user(_, role(user)).   % everyone logged in is a user
user(bob, password(secret)).

		 /*******************************
		 *            EXPAND		*
		 *******************************/

:- http_request_expansion(user, 100).
:- http_request_expansion(rbac, 200).

%!  user(+Request0, -Request, +Options) is semidet.
%
%   HTTP request rewriter that figures out whether someone is logged in.
%   using this technique we can use   different  techniques to establish
%   the logged in status.

user(Request0, Request, _Options) :-
    http_in_session(_),
    http_session_data(user(User)),
    Request = [user(User)|Request0].


%!  rbac(+Request0, -Request, +Options) is semidet.
%
%   Establish whether the user  may  proceed   if  the  handler  options
%   contain a term role(Role).  Acts as follows:
%
%     1. If the user is logged in
%        - If the user has the desired role, succeed.
%        - Otherwise indicate the user is not authorized.  The
%          3rd argument of the `http_reply` exception provides
%          arbitrary context for the error page.
%     2. Otherwise redirect to the login page

rbac(Request, Request, Options) :-
    memberchk(role(Role), Options),
    (   memberchk(user(User), Request)
    ->  (   user(User, role(Role))
        ->  true
        ;   memberchk(path(Path), Request),
            throw(http_reply(forbidden(Path), [], [no_role(User, Role)]))
        )
    ;   memberchk(request_uri(Return), Request),
        http_link_to_id(login_form,
                        [ reason('The requested location requires login'),
                          return_to(Return)
                        ], HREF),
        http_redirect(see_other, HREF, Request)
    ).


		 /*******************************
		 *            ERROR		*
		 *******************************/

:- multifile
    http:status_page/3.

%!  http:status_page(+Term, +Context, -HTML)
%
%   Provide a custom error page for the forbidden action.

http:status_page(forbidden(Path), Context, HTML) :-
    phrase(page([ title('Access denied')
                ],
                [ h1('Access denied'),
                  p(['You do not have sufficient privileges to access ',
                     Path]),
                  \forbidden_reason(Context)
                ]),
           HTML).

forbidden_reason(Context) -->
    { memberchk(no_role(User, Role), Context) },
html(p('The user ~p does not have role ~p'-[User,Role])).








