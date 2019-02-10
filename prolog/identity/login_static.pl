:- module(login_static, []).
/** <module> Convenience method that creates a standard set of static file servers
 */

:- use_module(library(http/html_head)).
:- use_module(library(http/http_files)).
:- use_module(library(http/http_dispatch)).

:- multifile http:location/3.
:- dynamic   http:location/3.

http:location(js, '/js', []).
http:location(img, '/icons', []).
http:location(css, '/css', []).

user:file_search_path(js, '../prolog/web/js').
user:file_search_path(css, '../prolog/web/css').
user:file_search_path(img, '../prolog/web/icons').

:- http_handler(js(.), http_reply_from_files(js(.), []), [prefix]).
:- http_handler(css(.), http_reply_from_files(css(.), []), [prefix]).
:- http_handler(img(.), http_reply_from_files(img(.), []), [prefix]).
