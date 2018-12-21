:- module(login_page, [
          login_form//1,
          login_hidden_referer//0,
          login_user_name_field//0,
          login_password_field//0,
          login_remember_me_check//0,
          login_submit//0,
          login_warning//0,
          login_signup_link//0]).
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
:- use_module(library(http/http_wrapper)).
:- use_module(library(http/http_path)).
:- use_module(library(http/http_client)).
:- use_module(library(crypto)).
:- use_module(library(url)).


:- http_handler(login(.), login_form_handler,
                [id(login_form), identity(guest), priority(-100)]).

% TODO fix these, they're stubs so I can test
:- http_handler(login(dologin), do_login_handler, [id(dologin), identity(guest)]).
:- http_handler(login(signup), login_form_handler, [id(signup), identity(guest)]).
:- http_handler(login(forgot), login_form_handler, [id(forgot), identity(guest)]).


login_form_handler(_Request) :-
      reply_html_page(
          title('Login Form Page'),
          \login_form_page).

:-html_meta login_form(html, ?, ?).

login_form(Contents) -->
    html(form([class(login),
               method('POST'),
               action(location_by_id(dologin))],
              Contents
         )).

% TODO export these
%  TODO all the UX nits
%  TODO validation
login_hidden_referer -->
    {
        (   http_current_request(Request),
            memberchk(search(Search), Request),
            memberchk(redirect=Referer, Search)
        ;
            http_absolute_location(root(.), Referer, [])
        )  % TODO test that this uriencodes properly
    },
    html(input([type(hidden), name(referer), value(Referer)], [])).


login_warning -->
    {
        (   http_current_request(Request),
            memberchk(search(Search), Request),
            memberchk(warn=Warn, Search)
        )
    },
      html(div([id(warningarea), class([warn, active])], [Warn])).
login_warning --> [].

login_user_name_field -->
    html(input([type(text),
                name(uname),
                placeholder('User Name'),
                required])).

login_password_field -->
    html(input([type(password),
                name(passwd),
                placeholder('Password'),
               required])).

login_remember_me_check -->
    html(input([type(checkbox), name(rememberme)])).

login_submit -->
    html(input([type(submit),
                name(submit),
                value('Log In')])).

login_forgot_password -->
      html(a(href(location_by_id(forgot)), ['forgot password or user name'])).

login_form_page -->
    html(\login_form([
              \login_hidden_referer,
              div(\login_signup_link),
              \login_warning,
              div([label(for(uname), 'User Name:'), \login_user_name_field]),
              div([label(for(passwd), 'Password:'), \login_password_field]),
              div([\login_remember_me_check, 'Remember me']),
              div(\login_forgot_password),
              div(\login_submit)
          ])).

login_signup_link -->
    html(a(href(location_by_id(signup)),
                    'Register')).

		 /*******************************
		 *          Do Login            *
		 *******************************/


do_login_handler(Request) :-
        member(method(post), Request),
        !,
        % TODO handle errors so this doesn't fail
        http_read_data(Request, Data, []),
        member(referer=SuccessURL, Data),
        member(uname=UserName, Data),
        member(passwd=Password, Data),
        authenticate_user(UserName, Password),
        make_login_cookie(UserName, Cookie),
        format('Status: 302 Found~n'),
        format('Location: ~w~n', [SuccessURL]),
        % session cookie, lives until browser exits
        % however the cookie itself contains an expiry
        % as well
        format('Set-Cookie: login=~w; Path=/~n', [Cookie]),
        format('Content-type: text/plain~n~n').

authenticate_user(annie, _).  % TODO stub for testing

make_login_cookie(UName, Cookie) :-
      www_form_encode(URLEncodedUName, UName),
      get_time(Now),
      Expires is floor(Now) + 86400,
      identity:get_crypto_key(Key),
      atomics_to_string([URLEncodedUName, "/", Expires], PlainText),
      crypto_n_random_bytes(12, Nonce),
      crypto_data_encrypt(PlainText, 'chacha20-poly1305',
                          Key, Nonce, CipherText, []),
      string_codes(CipherText, CipherTextCodes),
      append(Nonce, CipherTextCodes, TokenList),
      hex_bytes(Cookie, TokenList).

