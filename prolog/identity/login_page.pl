:- module(login_page, [
          login_form//1,
          login_reason//0,
          login_hidden_referer//0,
          login_user_name_field//0,
          login_password_field//0,
          login_remember_me_check//0,
          login_submit//0,
          login_remember_submit//0,
          login_warning//0,
          login_register_link//0,
          do_actual_login/4]).
/** <Module> Login page
*
* The page that presents a login form for the identity pack.
*
* To make a custom login page, define a new handler like
*
* :- http_handler(login(.), my_login_form_handler,
*                [id(login_form), identity(guest), priority(10)]).
*
* and copy login_form_handler to make my_login_form_handler.
*
*/
:- use_module(library(http/http_dispatch)).
:- use_module(library(http/html_write)).
:- use_module(library(http/http_wrapper)).
:- use_module(library(http/http_parameters)).
:- use_module(library(http/http_session)).
:- use_module(library(identity/login_database)).
:- ensure_loaded(library(identity/login_register)).
:- use_module(library(identity/customize)).
:- use_module(library(http/http_header)).
:- use_module(library(identity/login_remember), [remember_cookie_contents/2]).

:- http_handler(login(.), login_form_handler,
                [id(login_form), priority(-100)]).
:- http_handler(login(dologin), do_login_handler,
                [id(dologin)]).

login_form_handler(_Request) :-
      setting(identity:style, Style),
      reply_html_page(
          Style,
          title(\local('Login Form')),
          \login_form_page).

:-html_meta login_form(html, ?, ?).

login_form(Contents) -->
    html(form([class(login),
               method('POST'),
               action(location_by_id(dologin))],
              Contents
         )).

%  TODO all the UX nits
%  TODO validation
login_hidden_referer -->
    {
        (   http_current_request(Request),
            memberchk(search(Search), Request),
            memberchk(return_to=Referer, Search)
        ;
            http_location_by_id(home, Referer)  % TODO test this line
        )  % TODO test that this uriencodes properly
    },
    html(input([type(hidden), name(referer), value(Referer)], [])).


login_warning -->
    {
        (   http_current_request(Request),
            memberchk(search(Search), Request),
            memberchk(warn=Warn, Search)
        )
    },
      html(div([id(warningarea), class([warn, active])], [Warn])).
login_warning --> [].

login_user_name_field -->
    html(input([type(text),
                name(uname),
                placeholder('User Name'),
                required])).

login_password_field -->
    { local('Password', Placeholder) },
    html(input([type(password),
                name(passwd),
                placeholder(Placeholder),
               required])).

login_remember_me_check -->
    html(input([type(checkbox), name(rememberme), value(yes)])).

login_submit -->
    {  local('Log In', Submit) },
    html(input([type(submit),
                name(loginbutton),
                value(Submit)])).

login_remember_submit -->
    {  local('Log in until I close my browser', Submit),
       setting(identity:rememberme_duration, DurSecs),
       Days is round(DurSecs / 86400.0),
       local('Log me in for ~w days', Local),
       format(atom(SubmitDays), Local , [Days])
    },
    html([
          input([type(submit),
                name(loginbutton),
                value(Submit)]),
          input([type(submit),
               name(rememberloginbutton),
               value(SubmitDays)])
         ]).

login_forgot_password -->
      html(a(href(location_by_id(forgot)),
             [\local('forgot password or user name')])).

login_form_page -->
    html(\login_form([
              \login_hidden_referer,
              div(\login_reason),
              div(\login_register_link),
              \login_warning,
              div([label(for(uname), \local('User Name:')),
                   \login_user_name_field]),
              div([label(for(passwd), \local('Password:')),
                   \login_password_field]),
              div([\login_remember_me_check, \local('Remember me')]),
              div(\login_forgot_password),
              div(\login_submit),
              div(\login_remember_submit)
          ])).

login_register_link -->
    html(a(href(location_by_id(register)),
                    'Register')).

login_reason -->
      { http_current_request(Request),
        http_parameters(
            Request,
            [ reason(Reason, [default('')])
            ])
      },
      login_reason(Reason).

login_reason('') --> [].
login_reason(Reason) -->
      html(p(class('login-reason'), \local(Reason))).

		 /*******************************
		 *          Do Login            *
		 *******************************/


do_login_handler(Request) :-
        http_parameters(
            Request,
            [ referer(SuccessURL, [default(root(.))]), % is this default ok?
              uname(UserName, []),
              passwd(Password, []),
              rememberme(RememberMe, [default(no)]),
              rememberloginbutton(RememberLogin, [default(notthatbutton)])
            ]),
        authenticate_user(UserName, Password, Status),
        new_status_if_remembering(Status,
                                  RememberMe,
                                  RememberLogin,
                                  NewStatus),
        do_actual_login(NewStatus, SuccessURL, UserName, Request).
do_login_handler(_Request) :-
      setting(identity:style, Style),
      reply_html_page(
          Style,
          title(\local('improper login')),
          \improper_login).

% TODO this is public, pldoc it
% TODO Let Jan know - throwing is awkward for making links that
% are disabled/invisible if the user can't access them.
%
%  TODO check that this can be overridden
%
% TODO move ERROR section to it's own module and update README.md
%
% TODO make rest endpoints work, or at least test that they do
% make sure pengines work
do_actual_login(ok, SuccessURL, UserName, Request) :-
      http_open_session(_SessionId, []),
      http_session_assert(user(UserName)),
      http_redirect(see_other, SuccessURL, Request).
do_actual_login(ok_remember, SuccessURL, UserName, _Request) :-
      http_open_session(_SessionId, []),
      http_session_assert(user(UserName)),
      remember_cookie_contents(UserName, Contents),
      http_status_reply(see_other(SuccessURL),
      %    see_other('http://localhost:5000/secret'),
                        current_output,
                       ['Set-Cookie'(Contents)], % []  DEBUGGING
                        _).
do_actual_login(Status, SuccessURL, _UserName, Request) :-
      Status \= ok,
      Status \= ok_remember,
      http_link_to_id(login_form,
                      [
                          warn(Status),
                          referer(SuccessURL)
                      ],
                      HREF),
      http_redirect(see_other, HREF, Request).

new_status_if_remembering(ok, yes, _, ok_remember).
new_status_if_remembering(ok, _, Button, ok_remember) :-
      Button \= notthatbutton.
new_status_if_remembering(Status, _, _, Status).

wants_rememberme(Request) :-
      member(search(S), Request),
      member(rememberme=yes, S).
wants_rememberme(Request) :-
      member(search(S), Request),
      member(rememberloginbutton=_, S).

improper_login -->
      html(
          div(class('improper-login'),
              [
                  h1(\local('Sorry, login request ill formed')),
                  a(href(location_by_id(home), \local('Return to home')))
              ])).
