:- module(identity, [
              identity_dispatch/2
          ]).
/** <module> identity - pack to manage user identities on the SWI-Prolog web framework.
 *
 *  this pack depends on OpenSSL1.1.0 or greater
 */

:- use_module(library(http/http_dispatch)).
:- use_module(library(http/http_path)).
:- use_module(library(http/http_session)).
:- use_module(library(crypto)).
:- use_module(library(url)).

:- multifile http:location/3.
:- dynamic   http:location/3.

http:location(login, root(login), [priority(-100)]).

:- use_module(library(identity/login_page)).

:- meta_predicate identity_dispatch(1, +).
%!  identity_dispatch is det
%
%   A replacement dispatcher for the stock SWI-prolog dispatcher
%   http_dispatch.
%
%   Provides identity management services.
%   **Identity** in this context is a username or email. The pack
%   provides services like login, logout, knowing if user is allowed
%   to visit this handler, etc.
%
%   TODO need description of how we decide if they can access
%
%   If a visitor requests a page to which they do not have access,
%   a 302 Found response will be generated. This 302 will redirect
%   to the abstract location login(.)
%
%   If the Referer header is present, and on the same domain, then
%   a message about 'you must be logged in to access that page' is
%   presented in the login page.
%
%   The login page itself is on a low priority handler. To make a custom
%   login page, use the provided DCGs in identity/login_page and
%   make your own.
%
%   usage:
%
%   http_server(identity_dispatch(http_dispatch), [.. server options
%   ...]).
%
identity_dispatch(OriginalDispatch, Request) :-
    request_pagerole(Request, PageRole),
    identity_dispatch_role(PageRole, OriginalDispatch, Request).

identity_dispatch_role(guest, OriginalDispatch, Request) :-
     call(OriginalDispatch, Request).
identity_dispatch_role(user, OriginalDispatch, Request) :-
    memberchk(cookie(Cookies), Request),
    memberchk(login=Token, Cookies),
    (   token_uname(Token, Uname)
    ->  call(OriginalDispatch, [identity(Uname) |Request])
    ;   redirect_to_login(Request)
    ),
    !.
identity_dispatch_role(user, _, Request) :-
    redirect_to_login(Request).
identity_dispatch_role(Role, OriginalDispatch, Request) :-
    \+ member(Role, [guest, user]),
    memberchk(cookie(Cookies), Request),
    memberchk(login=Token, Cookies),
    (   token_uname(Token, Uname),
        user_has_role(Uname, Role)
    ->  call(OriginalDispatch, [identity(Uname) |Request])
    ;   redirect_to_login(Request)
    ),
    !.

% another approach to providing the redirect is to use a cookie

:-multifile identity:special_role/2.

user_has_role(Uname, Role) :-
    once(identity:special_role(Uname, Role)).

redirect_to_login(Request) :-
    memberchk(path(RedirPath), Request), % place to return to
    www_form_encode(RedirPath, URIEncRedirPath),
    http_absolute_location(login(.), Path, []),
    atomic_list_concat([Path, '?redirect=', URIEncRedirPath], URL),
    http_redirect(moved_temporary,
                  URL,
                  Request).

% TODO provide login page in login_page module

%!  request_pagerole(+Request:request, -PageRole:atom) is det
%
%   determine the pagerole of this page (contents of identity option)
%
request_pagerole(Request, PageRole) :-
    member(path(Path), Request),
    http_dispatch:find_handler(Path, _Handler, Options),
    (   member(identity(PageRole), Options)
    ;   PageRole = guest
    ).

% TODO move this and make_login_cookie to their own module

%!  token_uname(+Token:string, -Uname:text) is semidet
%
%   Succeeds returning the user name if the token is valid
%   or fails if not
%
token_uname(Token, Uname) :-
    hex_bytes(Token, TokenList),
    length(Nonce, 12),
    append(Nonce, CipherTextCodes, TokenList),
    string_codes(CipherText, CipherTextCodes),
    get_crypto_key(Key),
    crypto_data_decrypt(CipherText,
                        'chacha20-poly1305',
                        Key,
                        Nonce,
                        RecoveredText,
                        []),
    split_string(RecoveredText, "/", "", [URLEncodedUName, ExpiresUtimeString]),
    number_string(Expires, ExpiresUtimeString),
    get_time(Now),
    Expires > Now,
    www_form_encode(URLEncodedUName, Uname).

:- dynamic crypto_key/1.

get_crypto_key(Key) :-
    crypto_key(Key),
    !.
get_crypto_key(Key) :-
    catch(
        setup_call_cleanup(
            open('secret_identity_key', read, Stream),
            read(Stream, Key),
            close(Stream)
        ),
        error(existence_error(source_sink, _), _),
        (   create_crypto_key_file, % make sure we really wrote it
            get_crypto_key(Key),
            asserta(crypto_key(Key))
        )
    ).

create_crypto_key_file :-
    crypto_n_random_bytes(32, Key),
    setup_call_cleanup(
        open('secret_identity_key', write, Stream),
        (   writeq(Stream, Key),
            write(Stream, '.'),
            flush_output(Stream)
        ),
        close(Stream)
    ).












