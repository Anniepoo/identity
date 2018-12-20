:- module(login_page, []).
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

:- http_handler(login(.), login_form_handler,
                [id(login_form), identity(guest), priority(-100)]).

% TODO fix these, they're stubs so I can test
:- http_handler(login(dologin), login_form_handler, [id(dologin), identity(guest)]).
:- http_handler(login(signup), login_form_handler, [id(signup), identity(guest)]).


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

% TODO export these
%  TODO all the UX nits
%  TODO validation
login_hidden_referer -->
    {
        (   http_current_request(Request),
            memberchk(search(Search), Request),
            memberchk(redirect=Referer, Search)
        ;
            http_absolute_location(root(.), Referer, [])
        )  % TODO test that this uriencodes properly
    },
    html(input([type(hidden), name(referer), value(Referer)], [])).

login_user_name_field -->
    html(input([type(text), name(uname)])).

login_password_field -->
    html(input([type(password), name(passwd)])).

login_remember_me_check -->
    html(input([type(checkbox), name(rememberme)])).

login_submit -->
    html(input([type(submit), name(submit), value(submit)])).


login_form_page -->
    html(\login_form([
              \login_hidden_referer,
              div(\login_signup_link),
              div(id(unamewarning), []),
              div([label(for(uname), 'User Name:'), \login_user_name_field]),
              div([label(for(passwd), 'Password:'), \login_password_field]),
              div([\login_remember_me_check, 'Remember me']),
              div(\login_submit)
          ])).

login_signup_link -->
    html(a(href(location_by_id(signup)),
                    'Sign up')).
