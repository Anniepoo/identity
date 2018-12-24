:- module(login_page, [
          login_form//1,
          login_hidden_referer//0,
          login_user_name_field//0,
          login_password_field//0,
          login_remember_me_check//0,
          login_submit//0,
          login_warning//0,
          login_register_link//0]).
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
:- use_module(library(http/http_path)).
:- use_module(library(http/http_client)).
:- use_module(library(identity/login_crypto)).
:- use_module(library(identity/login_database)).
:- ensure_loaded(library(identity/login_register)).

:- http_handler(login(.), login_form_handler,
                [id(login_form), identity(guest), priority(-100)]).
:- http_handler(login(dologin), do_login_handler,
                [id(dologin), identity(guest)]).

% TODO fix these, they're stubs so I can test

:- http_handler(login(forgot), login_form_handler, [id(forgot), identity(guest)]).


login_form_handler(_Request) :-
      reply_html_page(
          title('Login Form Page'),
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
            memberchk(redirect=Referer, Search)
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
    html(input([type(password),
                name(passwd),
                placeholder('Password'),
               required])).

login_remember_me_check -->
    html(input([type(checkbox), name(rememberme)])).

login_submit -->
    html(input([type(submit),
                name(submit),
                value('Log In')])).

login_forgot_password -->
      html(a(href(location_by_id(forgot)), ['forgot password or user name'])).

login_form_page -->
    html(\login_form([
              \login_hidden_referer,
              div(\login_register_link),
              \login_warning,
              div([label(for(uname), 'User Name:'), \login_user_name_field]),
              div([label(for(passwd), 'Password:'), \login_password_field]),
              div([\login_remember_me_check, 'Remember me']),
              div(\login_forgot_password),
              div(\login_submit)
          ])).

login_register_link -->
    html(a(href(location_by_id(register)),
                    'Register')).

		 /*******************************
		 *          Do Login            *
		 *******************************/


do_login_handler(Request) :-
        member(method(post), Request),
        http_read_data(Request, Data, []),
        member(referer=SuccessURL, Data),
        member(uname=UserName, Data),
        member(passwd=Password, Data),
        authenticate_user_status(UserName, Password, Status),
        www_form_encode(Status, URLStatus),
        (   Status = ok
        ->
            make_login_cookie(UserName, Cookie),
            format('Status: 302 Found~n'),
            format('Location: ~w~n', [SuccessURL]),
            % session cookie, lives until browser exits
            % however the cookie itself contains an expiry
            % as well
            format('Set-Cookie: login=~w; Path=/~n', [Cookie]),
            format('Content-type: text/plain~n~n')
        ;
            http_location_by_id(login_form, LoginPage),
            format('Status: 302 Found~n'),
            format('Location: ~w?warn=~w~n', [LoginPage, URLStatus]),
            format('Content-type: text/plain~n~n')
        ).
do_login_handler(_Request) :-
      reply_html_page(
          title('improper login'),
          \improper_login).

improper_login -->
      html(
          div(class('improper-login'),
              [
                  h1('Sorry, login request ill formed'),
                  a(href(location_by_id(home), 'Return to home'))
              ])).
