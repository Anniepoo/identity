:- module(login_remember, [
              remember_me_expand/3,
              remember_cookie_contents/2
          ]).
/** <module> Remember Me check functionality
 *
 * This is internal functionality for the remember me checkbox
 *
 * Not working yet
 *
 * Design:
 *
 * if remember_me is checked, do_actual_login provides an additional
 * cookie using http_status_reply/4  YET TO DO
 *
 * Provide an additional http_request_expansion in identity.pl
 * with priority 150. If the user  provides a remember_me cookie
 * and is not logged in,
 * do a redirect to a special version of login_page. This
 * will look at the remember-me and then call do_actual_login
 * with an ok status code and a referer of the original page.
 * (didn't actually do it, as we don't want to reissue  the
 * remember  cookie if logged in this way. Just do the login stuff)
 * if the cookie's missing or invalid just succeed.
 *
 * logout link will kill the remember_me cookie.
 *
 * The remember_me checkbox html//1 inclusion moves here?? No.
 *
 */
:- use_module(library(identity/login_crypto)).
:- use_module(library(http/http_session)).
:- use_module(library(http/http_dispatch)).

%!  remember_me_expand(+Request0, -Request, +Options) is semidet.
%
%   HTTP request rewriter that figures out whether request
%   has a current, valid persistent 'remember me' cookie.
%
%   If the user is already logged in, we do nothing.
%   If the user is not logged in, and we have a valid
%   cookie, we log the user in. This happens on any
%   page, even those with no role.
%
%   @arg Request0  the original request
%   @arg Request   Bound to the new request on exit
%   @arg Options   The last argument of http_handler/3
%
remember_me_expand(Request, Request, _Options) :-
    member(user(_), Request),
    !.
remember_me_expand(Request, Request, _Options) :-
    \+ member(user(_), Request), % we're not logged in
    valid_remember_me_cookie(Request, User),
    member(request_uri(URL), Request),
    http_open_session(_SessionId, []),
    http_session_assert(user(User)),
    http_redirect(see_other, URL, Request).
remember_me_expand(Request, Request, _Options).

valid_remember_me_cookie(Request, User) :-
    member(cookie(Cookies), Request),
    member('login'=Token, Cookies),
    token_uname(Token, User). % this checks the time

remember_cookie_contents(UserName, Contents) :-
    make_login_cookie(UserName, Token),
    setting(identity:rememberme_duration, DurSecs),
    (   http_session_option(samesite(lax))
    ->
        format(atom(Contents), 'login=~w; Max-Age=~d; SameSite=Lax; Path=/',
           [Token, DurSecs])
    ;
        format(atom(Contents), 'login=~w; Max-Age=~d; SameSite=Strict; Path=/',
           [Token, DurSecs])
    ).

% Note - use http_status_reply from library(http/http_header)
% TODO add expire to the cookie itself, this is a session cookie
%
        /*  TODO - make the 'remember me' cookie if remember me checked
            make_login_cookie(UserName, Cookie), % TODO now only send cookie if
            format('Status: 302 Found~n'),       %
            format('Location: ~w~n', [SuccessURL]),
            % session cookie, lives until browser exits
            % however the cookie itself contains an expiry
            % as well
            format('Set-Cookie: login=~w; Path=/~n', [Cookie]),
            format('Content-type: text/plain~n~n')
        */


 % TODO - make sure logout kills the remember me cookie

% TODO - what should default state be for 'remember me?'
% Use the 'two buttons solution'

% TODO add login_reason to handler options
