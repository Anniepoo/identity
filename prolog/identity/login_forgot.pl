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

% where the 'forgot my pw' link goes
:- http_handler(login(forgot), forgot_form_handler, [priority(-100), id(forgot)]).
% where user goes after entering email at form on above page
:- http_handler(login(doforgot), doforgot_form_handler, [priority(-100), id(doforgot)]).
% Where the emailed link takes the user
:- http_handler(login(resetpw), resetpw_form_handler, [prefix, priority(-100), id(resetpw)]).


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
                 p([input([name(email), type(text)]),
                 input([name(submit), type(submit), value('Send')])])
             ])
    ]).

% doforget landing page, Where you go after entering your
% email
doforgot_form_handler(Request) :-
      setting(identity:style, Style),
      http_parameters(
            Request,
            [ email(Email)
            ]),
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
      www_form_encode(UName, URIUName),
      www_form_encode(Key, URIKey),
      reply_html_page(
          Style,
          title(\local('Reset Password')),
          \resetpw_form_page(UName, Key)).

resetpw_form_page(UName, Key) -->
      html([p('TODO')]).


% TODO finish the page with the the reset password form, id(resetpw)

% and make
% the page it lands on.





