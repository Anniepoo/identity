:- module(identity, [
          ]).

/** <module> identity - pack to manage user identities on the SWI-Prolog web framework.
 *
 *  this pack depends on OpenSSL1.1.0 or greater
 */

:- use_module(library(http/http_dispatch)).
:- use_module(library(http/html_write)).
:- use_module(library(http/http_session)).

:- multifile http:location/3.
:- dynamic   http:location/3.

http:location(login, root(login), [priority(-100)]).

:- ensure_loaded(library(identity/customize)).
:- ensure_loaded(library(identity/login_page)).
:- ensure_loaded(library(identity/logout)).
:- use_module(library(identity/login_database), [user_property/2]).

		 /*******************************
		 *            EXPAND		*
		 *******************************/

% order is important, need user field in second
:- http_request_expansion(user_expand, 100).
:- http_request_expansion(role_based_authorization_expand, 200).

%!  user_expand(+Request0, -Request, +Options) is semidet.
%
%   HTTP request rewriter that figures out whether someone is logged in.
%   using this technique we can use   different  techniques to establish
%   the logged in status.
%
%   If the user is logged in, we add user(User) to the request
%
user_expand(Request0, Request, _Options) :-
    http_in_session(_),
    http_session_data(user(User)),
    Request = [user(User)|Request0].


%!  role_based_authorization_expand(+Request0, -Request, +Options) is semidet
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

role_based_authorization_expand(Request, Request, Options) :-
    memberchk(role(Role), Options),
    (   memberchk(user(User), Request)
    ->  (   user_property(User, role(Role))
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


% TODO Let Jan know - throwing is awkward for making links that
% are disabled/invisible if the user can't access them.
% (not really Jan's problem)
%
%  TODO check that this can be overridden
%
% TODO move ERROR section to it's own module and update README.md
%
% TODO make rest endpoints work, or at least test that they do
% make sure pengines work


