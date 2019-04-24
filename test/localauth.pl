:- module(localauth, [go/0]).
/** <module>  Simple OAuth2 server to test OAuth2
 *
 * This is a minimal OAuth2 server implementation so you can test
 * an OAuth2 client.
 *
 * The Authorization provider is, instead of Google or Facebook,
 * this little application, which I've named "DodgyAuth"
 *
 * It happily will validate to an http address.
 * Saves the hassle of setting up a cert for localhost.
 *
 * This is not cryptographic anything. It just implements the basics
 * of the protocol. DO NOT actually use it for anything but testing your
 * OAuth2 client works.
 *
 * swipl test.pl -g go
 *
 * How OAuth2 works:
 * Alice (user) wants to verify her identity to Bob (application). Both
 * Alice and Bob trust Carl(OAuth provider).
 * Bob gives Alice a non secret client ID (Bob's 'name') and a link to
 * Carl. Alice tells Carl 'it's OK to tell Bob I'm me'.
 * Carl redirects Alice to Bob, with a token(`auth code`).
 * Alice gives the `auth code` to Bob. Bob gives Carl
 * the auth code, and Carl gives Bob back an `access token`.
 * Now Bob can use the access token to get permission to
 * see Alice's stuff on some third party.
 *
 * Of course the third party could be Bob's own user login system.
 *
The
 and carl hands
 * her a token. Alice gives the token to Bob. Bob gives the token to

[Handy OAuth2
 * explainer](https://aaronparecki.com/oauth-2-simplified/#roles)
 *
 * Your client ID is 42 for sketchyapp
 * The scope is defined by the provider. DodgyAuth displays the scope
 * but doesn't use it for anything.
 *
 *
 *
 *
 */

:- use_module(library(http/thread_httpd)).
:- use_module(library(http/http_dispatch)).
:- use_module(library(http/html_write)).
:- use_module(library(http/js_write)).
:- use_module(library(http/http_json)).

user:file_search_path(library, '../prolog').
:- use_module(library(identity/login_static)).

go :-
    current_prolog_flag(version, X),
    X >= 80100,
    use_default_db,
    http_set_session_options(
        [ create(noauto),
          timeout(60)  % half hour sessions
        ]),
    http_server(http_dispatch, [port(6666)]).
go :-
    writeln('Need to be on SWI-Prolog 8.1.0 or better, you are on'),
    version.

:- http_handler(root(loginform), login_form_handler, [id(loginform)]).

login_form_handler(_Request) :-
      reply_html_page(
          title('Login with DodgyAuth!'),
          [h1('Login with DodgyAuth'),
           a(href('http://localhost:5000), 'Login as Alice'),
           a(href(location_by_id(logout)), 'Log Out'),
           div(id(loadbyajax), 'not yet loaded by ajax'),
           p(\current_user),
           \js_script({| javascript(_) ||
     fetch("/ajax").then(function(response) {
                             return response.json();
                         })
                       .then(function(myJson) {
                                 document.getElementById("loadbyajax").innerHTML = myJson.displaytext;
                             });
              |})
          ]).

:- http_handler(root(ajax), ajax_handler, [id(ajax), role(user)]).

ajax_handler(_Request) :-
    reply_json_dict(_{
                        displaytext: 'AJAX fetched me correctly'
                    }).

		 /*******************************
		 * HEY - If you take this test code and
		 * build an app using it as a starter,
                 * that's ok, and reasonable, but
		 * GET RID OF THIS SECTION
		 * the test framework uses it to reset
		 * the user DB between tests
		 * ******************************/

:- http_handler(root(resetdb), reset_db, [id(resetdb)]).

reset_db(Request) :-
    % TODO restrict to localhost
    retractall_user_property(_, _),
    (   http_in_session(ID)
    ->  http_close_session(ID)
    ;   true
    ),
    reply_html_page(
        title('zapped the db'),
        p('You have truncated the user database')
    ).

