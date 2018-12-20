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

:- http_handler(login(.), login_form_handler,
                [id(login_form), identity(guest), priority(-100)]).

login_form_handler(_Request) :-
      reply_html_page(
          h1('Login Form Page'),
          p('login form')).
