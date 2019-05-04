:- module(seeotherdemo, [go/0]).

:- use_module(library(http/thread_httpd)).
:- use_module(library(http/http_dispatch)).
:- use_module(library(http/html_write)).
:- use_module(library(http/http_session)).
:- use_module(library(http/http_header)).

go :-
    current_prolog_flag(version, X),
    X >= 80100,
    http_set_session_options(
        [ create(noauto),
          timeout(1800)  % half hour sessions
        ]),
    http_server(http_dispatch, [port(9000)]).
go :-
    writeln('Need to be on SWI-Prolog 8.1.0 or better, you are on'),
    version.

:- http_handler(root(.), manual_handler, [id(home)]).
:- http_handler(root(foo), foo_handler, [id(foo)]).

root_handler(_Request) :-
      http_status_reply(
          moved('http://localhost:9000/foo'),
                        current_output,
                 [], %       ['Set-Cookie'(Contents)],   DEBUGGING
                        _).

foo_handler(_Request) :-
       reply_html_page(
           title(foo),
           h1('the foo page')).

manual_handler(_Request) :-
        format('Status: 303 See Other~n'),
        format('Content-type: text/plain~n'),
        format('Location: http://localhost:9000/foo~n~n'),
        format('Hello World!~n'),
        close(current_output).
