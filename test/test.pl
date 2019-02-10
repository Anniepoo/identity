:- module(testcondition, [go/0]).
/** <module>  Integration tests for identity library
 *
 * Run these tests from the directory where this file is found
 *
 * swipl test.pl -g run_tests.
 */

:- use_module(library(http/thread_httpd)).
:- use_module(library(http/http_dispatch)).
:- use_module(library(http/html_write)).
:- use_module(library(http/http_session)).

user:file_search_path(library, '../prolog').

:- use_module(library(identity/identity)).
:- use_module(library(identity/login_database), [use_default_db/0,
                                                current_user//0]).
:- use_module(library(identity/login_static)).

go :-
    use_default_db,
    http_set_session_options(
        [ create(noauto),
          timeout(1800)  % half hour sessions
        ]),
    http_server(http_dispatch, [port(5000)]).

:- http_handler(root(.), root_handler, [id(home)]).
:- http_handler(root(secret), secret_handler, [id(secret), role(user)]).

root_handler(_Request) :-
      reply_html_page(
          h1('Home Page'),
          a(href(location_by_id(secret)), 'link to secret')).

secret_handler(_Request) :-
      reply_html_page(
          h1('Secret Page'),
          [a(href(location_by_id(home)), 'link to home page'),
           a(href(location_by_id(logout)), 'Log Out'),
           p(\current_user)]).

