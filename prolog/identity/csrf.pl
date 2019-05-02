:- module(csrf, [ check_csrf/2,
                  csrf_field//1
                ]).
/** <module> Cross Site Resource Forgery prevention
 *
 * Use of session cookies to control access to APIs on the
 * server is subject to an exploit called Cross Site Resource Forgery.
 *
 * Suppose a user visits good.org, which has some API
 * https://good.org/sendmoney
 *
 * Afterwards the user visits evil.com. evil.com sends javascript
 * that executes an AJAX call to https://good.org/sendmoney.
 * Because the user's session is still active, the session cookie
 * for good.org is sent, and the AJAX call is authenticated.
 *
 * The solution is for good.org to include a hidden field
 * in the form that drives the ajax call. This field provides
 * a cryptographically secure random number that must match
 * data in the user's session to accept the call.
 *
 * evil.com cannot generate a valid AJAX request, as it does not know
 * the hidden value.
 *
 * Usage:
 *   To secure a REST endpoint, extract the csrf parameter
 *   from the request
 *   and pass it and the REST endpoint id to check_csrf/2.
 *   You should also add the normal role authorization.
 *
 *   Add csrf_field//1 to the form, passing the id of the REST handler
 */

:- use_module(library(crypto)).
:- use_module(library(http/http_session)).
:- use_module(library(http/html_write)).

%!   csrf_field(+ID:atom, ?a, ?b) is det
%
%    DCG that emits a hidden CSRF field
%
%    @arg ID  atom, the handler id for the REST endpoint of this form
%
csrf_field(ID) -->
    {
          crypto_n_random_bytes(64, Bytes),
          hex_bytes(HexAtom, Bytes),
          http_session_asserta(csrf(ID, HexAtom))
    },
    html(input([type(hidden), name(csrf), value(HexAtom)])).


check_csrf(ID, HexAtom) :-
    http_session_data(csrf(ID, HexAtom)).






















