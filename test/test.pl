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

user:file_search_path(library, '../prolog').

:- use_module(library(identity/identity)).

go :-
      http_server(identity_dispatch(http_dispatch), [port(5000)]).

:- http_handler(root(.), root_handler, [id(home), identity(guest)]).
:- http_handler(root(secret), secret_handler, [id(secret), identity(user)]).

root_handler(_Request) :-
      reply_html_page(
          h1('Home Page'),
          a(href(location_by_id(secret)), 'link to secret')).

secret_handler(_Request) :-
      reply_html_page(
          h1('Secret Page'),
          a(href(location_by_id(home)), 'link to home page')).

