:- module(login_register, []).
/** <module> Registration module for identity
 *
 */

:- use_module(library(http/http_dispatch)).
:- use_module(library(http/html_write)).
:- use_module(library(identity/login_page)).
:- use_module(library(http/http_client)).
:- use_module(library(http/http_path)).
:- use_module(library(identity/login_crypto)).
:- use_module(library(identity/login_database)).
:- use_module(library(identity/customize)).
:- use_module(library(identity/login_validate)).

:- http_handler(login(register), register_form_handler, [id(register), priority(-100)]).


register_form_handler(_Request) :-
    setting(identity:style, Style),
    reply_html_page(
        Style,
        title(\local('Register')),
        \register_body).

register_body -->
    html(div([
             h1(\local('Register')),
             div(\register_form)
             ])).

register_form -->
    html(
        form([method('POST'), action(location_by_id(doregister))],
             [
                 div(\login_warning),
                 \login_hidden_referer,
                 div([id(emailwarn), class(warning)], &(nbsp)),
                 div([label(for(email), \local('Email:')), \login_email]),
                 div([id(unamewarn), class(warning)], &(nbsp)),
                 div([label(for(uname), \local('User name:')),
                            \register_user_name_field]),
                 div([id(passwdwarn), class(warning)], &(nbsp)),
                 div([label(for(passwd), \local('Password:')),
                      \register_password_field]),
                 div([id(passwd2warn), class(warning)], \local('Field below must match password')),
                 div([label(for(passwd2), \local('Repeat Password:')),
                      \register_password2_field]),
                 \login_submit_register,
                 \validate_js
             ])).

login_submit_register -->
    html(input([type(submit),
                name(submit),
                value('Register')])).

register_user_name_field -->
    html(input([type(text),
                name(uname),
                placeholder('User Name'),
                oninput('doValidation(this)'),
                required])).

register_password_field -->
    { local('Password', Placeholder) },
    html(input([type(password),
                name(passwd),
                id(passwd),
                placeholder(Placeholder),
                oninput('doValidation(this)'),
               required])).

login_email -->
    { local('Email', Email) },
    html(input([type(email),
                name(email),
                placeholder(Email),
                oninput('doValidation(this)'),
                required])).

register_password2_field -->
    { local('Repeat Password', RP) },
    html(input([type(password),
                name(passwd2),
                id(passwd2),
                placeholder(RP),
                oninput('doValidation(this)'),
               required])).


		 /*******************************
		  *         doregister          *
		  *  handle register form data  *
		 *******************************/

:- http_handler(login(doregister), doregister_handler, [id(doregister), identity(guest), priority(-100)]).


doregister_handler(Request) :-
        member(method(post), Request),
        http_read_data(Request, Data, []),
        member(uname=UserName, Data),
        member(passwd=Password, Data),
        member(passwd2=Password2, Data),
        member(email=Email, Data),
        validate(ok, [uname=UserName,
                  passwd=Password,
                  passwd2=[Password, Password2],
                  email=Email],
                 Status),
        (   member(referer=SendUserTo, Data) ;
            http_location_by_id(home, SendUserTo) % TEST this
        ),
        (   Status == ok
        ->
            add_user(UserName,
                 Password,
                 Email
                 ),
            do_actual_login(ok, SendUserTo, UserName, Request)
        ;
           www_form_encode(Status, URLStatus),
           http_location_by_id(register, RegisterPage),
            format('Status: 302 Found~n'),
            format('Location: ~w?warn=~w~n', [RegisterPage, URLStatus]),
            format('Content-type: text/plain~n~n')
        ).

validate(MyStatus, [passwd2=[P,P] | Rest], Status) :-
    validate(MyStatus, Rest, Status).
validate(MyStatus, [passwd2=[P1,P2] | Rest], Status) :-
    P1 \= P2,
    atom_concat(MyStatus, 'Passwords Differ~n', NewStatus),
    validate(NewStatus, Rest, Status).

validate(Status, [], Status).
validate(ok, [FieldName=Value | Rest], Status) :-
    valid(FieldName=Value, MyStatus),
    validate(MyStatus, Rest, Status).
validate(OldStatus, [FieldName=Value | Rest], Status) :-
    valid(FieldName=Value, ok),
    validate(OldStatus, Rest, Status).
validate(OldStatus, [FieldName=Value | Rest], Status) :-
    valid(FieldName=Value, ThisStatus),
    atomic_list_concat([OldStatus, ' ', ThisStatus], NewStatus),
    validate(NewStatus, Rest, Status).

/*
        Status = ok,  %  TODO make this work probably with a throw
        www_form_encode(Status, URLStatus),
        (   Status = ok
        ->

            make_login_cookie(UserName, Cookie),
            format('Status: 302 Found~n'),
            format('Location: ~w~n', [SendUserTo]),
            % session cookie, lives until browser exits
            % however the cookie itself contains an expiry
            % as well
            format('Set-Cookie: login=~w; Path=/~n', [Cookie]),
            format('Content-type: text/plain~n~n')
        ;
            http_location_by_id(register, RegisterPage),
            format('Status: 302 Found~n'),
            format('Location: ~w?warn=~w~n', [RegisterPage, URLStatus]),
            format('Content-type: text/plain~n~n')
        ).
*/




