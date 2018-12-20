:- module(testcondition, [go/0]).

:- use_module(library(http/thread_httpd)).
:- use_module(library(http/http_dispatch)).
:- use_module(library(http/html_write)).

go :-
      http_server(http_dispatch, [port(5000)]).

:- http_handler(root(.) , first, [priority(100), tacos(onions)]).

% :- http_handler(root(.) , second, [condition(true_one), priority(0)]).

first(_R) :-
    reply_html_page(
        title(first),
        h1('First')).

second(_R) :-
    reply_html_page(
        title(second),
        h1('Second')).

fail_one(P) :-
    debug(testcondition(fail_one), 'fail_one ~w', [P]),
    !,
    fail.

true_one(P) :-
    debug(testcondition(true_one), 'true_one ~w', [P]).
