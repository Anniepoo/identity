:- module(login_forgot, [
          forgot_password_link//0]).
/** <Module> Forgot my password page
*
* The page that presents a reset my password form
*
* The link that causes a reset password email to be sent
* and a landing page displayed, with where you came from
*
*/
:- use_module(library(http/http_dispatch)).
:- use_module(library(http/html_write)).
:- use_module(library(http/http_parameters)).
:- ensure_loaded(library(identity/login_register)).
:- use_module(library(identity/login_email), [send_forgot_email/1]).
:- use_module(library(identity/customize)).
:- use_module(library(identity/login_crypto), [password_hash/2]).
:- use_module(library(identity/login_database), [user_property/2, set_user_property/2]).
:- use_module(library(http/http_client)).

:- setting(identity:reset_email_life, integer, 86400,
           "Time a password reset email is valid").

% where the 'forgot my pw' link goes
:- http_handler(login(forgot), forgot_form_handler, [priority(-100), id(forgot)]).
% where user goes after entering email at form on above page
:- http_handler(login(doforgot), doforgot_form_handler, [priority(-100), id(doforgot)]).
% Where the emailed link takes the user
:- http_handler(login(resetpw), resetpw_form_handler, [prefix, priority(-100), id(resetpw)]).
% Where the form presented in resetpw takes the user
:- http_handler(login(doactualpwreset), do_actual_reset_handler, [priority(-100),
                                                               id(doactualpwreset)]).

forgot_password_link -->
      html([
          a(href(location_by_id(forgot)), \local('Forgot my password'))
      ]).

% Where the 'Forgot password' link goes
forgot_form_handler(_Request) :-
      setting(identity:style, Style),
      reply_html_page(
          Style,
          title(\local('Gather Email')),
          \forgot_form_page).

% todo client side validation of this.
forgot_form_page -->
    html([
        form([method('POST'), action(location_by_id(doforgot))],
             [
                 h1(class('forgotheader'), \local('Password reset')),
                 p(
\local('Enter your email address. An email with a password reset link will be sent')),
                 p([input([name(email), type(email), required]),
                 input([name(submit), type(submit), value('Send')])])
             ])
    ]).

% doforget landing page, Where you go after entering your
% email
doforgot_form_handler(Request) :-
      setting(identity:style, Style),
      member(method(post), Request), !,
      http_read_data(Request, Data, []),
      member(email=Email, Data),
      send_forgot_email(Email),
      reply_html_page(
          Style,
          title(\local('Email Sent')),
          \doforgot_page).

doforgot_page -->
      html([h1(\local('Email Sent')),
            p(\local('An email has been sent to this address if that email has an account.'))
            % TODO should there be a back button? needs to go not 'back', but back 2
           ]).

% last two segments are /username/key
resetpw_form_handler(Request) :-
      setting(identity:style, Style),
      member(path(P), Request),
      reverse(P, [URIKey, URIUName | _]),
      reply_html_page(
          Style,
          title(\local('Reset Password')),
          \resetpw_form_page(URIUName, URIKey)).

resetpw_form_page(URIUName, URIKey) -->
      { local('New Password', NewPasswordPlaceholder),
        local('Repeat New Password', NewPasswordPlaceholder2),
        local('Change', CH)
      },
      html([h1(\local('Reset Password')),
           form([method('POST'), action(location_by_id(doactualpwreset))],
               [
                input([type(hidden), name(uname), value(URIUName)]),
                input([type(hidden), name(resetkey), value(URIKey)]),
                input([type(password), name(passwd), placeholder(NewPasswordPlaceholder)]),
                input([type(password), name(spasswd2), placeholder(NewPasswordPlaceholder2)]),
                input([type(submit), name(submit), value(CH)])
                ])]).

do_actual_reset_handler(Request) :-
      setting(identity:style, Style),
      http_parameters(Request,
                      [
                          uname(UName, []),
                          key(Key, []),
                          passwd(PassWD, []),
                          passwd2(PassWD2, [])
                      ]),
      (   PassWD = PassWD2,
          change_password(UName, Key, PassWD)
      ->
          http_location_by_id(login(login), SuccessURL),
          http_redirect(see_other, SuccessURL, Request)
      ;
          reply_html_page(
              Style,
              title(\local('Cannot Reset Password')),
              \bad_reset_page)
      ).

bad_reset_page -->
      html([
          h1(\local('Cannot Reset Password')),
          p('We cannot reset your password. Either you entered two unmatched passwords, or your password reset link has expired')
      ]).

% succeeds if we changed the pw
change_password(UName, Key, PassWD) :-
      user_property(UName, forgot_pw(Key=Time)), % also checks user exists
      get_time(Now),
      setting(identity:reset_email_life, Delta),
      Delta > Now - Time,
      password_hash(PassWD, Hash),
      set_user_property(UName, password_hash(Hash)).





