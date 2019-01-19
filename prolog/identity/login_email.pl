:- module(login_email,
          [
              send_activation_email/3
          ]).
/** <module> Predicates related to sending out activation emails
 *
 */
:- use_module(library(http/http_dispatch)).
:- use_module(library(http/http_parameters)).
:- use_module(library(identity/login_database)).
:- use_module(library(http/http_session)).

% TODO make the response code
% TODO make inclusion for email link
% TODO make 'resend link'
% TODO handle not activated, go to a not-activated page
% "hey, you need to answer your email (resend)"
%

:- multifile login_email:activation_email_hook/3.

send_activation_email(UName, Email, Key) :-
    activation_link(UName, Key, Link),
    login_email:activation_email_hook(UName, Email, Link),
    !.
send_activation_email(UName, Email, Key) :-
    activation_link(UName, Key, Link),
    debug(identity(email), 'Activation email to ~w link ~w',
          [UName, Email, Link]).

% TODO change to route variables for key
/*
 *
[21:36] <anniepoo> http_handler(api(user/id), api_handler(Request) , [role(user)]). and somehow one gets
[21:37] <anniepoo> http://example.com/api/42/678  routed and can find out user=42 and id=678?
[21:37] <RLa> http_handler(api(user/Id), api_handler(Id) , [role(user)]) <- like that
[21:37] <anniepoo> ah, cool
[21:37] <anniepoo> ok
[21:37] <RLa> it saves lots of manual parsing
[21:38] <anniepoo> so http://example.com/api/user/42  and Id becomes 42
[21:38] <anniepoo> Got it!
[21:38] <RLa> yes
*/
% TODO probably needs base option to not get a relative uri
activation_link(UName, Key, Link) :-
    http_link_to_id(login(activate/UName/Key), [], Link).

:- http_handler(login(activate/UName/Key), activate_user(UName, Key), [id(activate)]).

activate_user(UName, Key, Request) :-
    user_property(UName, activation_key(Key)),
    user_property(UName, role(needs_activation)),
    retractall_user_property(UName, role(needs_activation)),
    retractall_user_property(UName, activation_key(_)),
    assert_user_property(UName, role(user)),
    http_link_to_id(home, [], HREF),
    http_redirect(see_other, HREF, Request).

% TODO fix to route variable and no login
:- http_handler(login(resend/UName), resend_activation(UName),
                [id(resend)]).

resend_activation(UName, Request) :-
    assert_user_property(UName, role(needs_activation)),
    uuid(Key),
    assert_user_property(UName, activation_key(Key)),
    user_property(UName, email(Email)),
    send_activation_email(UName, Email, Key),
    http_link_to_id(home, [], HREF),
    http_redirect(see_other, HREF, Request).

