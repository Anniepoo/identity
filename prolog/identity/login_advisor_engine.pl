:- module(login_advisor_engine, [
          ps/0,
          ss/0,
          noss/0,
          init/0,
          decision/1,
              mumble/0,
          activity/1]).
/** <module> Engine code for the login advisor
 *
 */

:- use_module(library(chr)).


:- chr_constraint
    decision/1,
    task/1,
    security/1,
    completed/1,
    reset/0.

reset, decision(_) <=> true.
reset, task(_) <=> true.
reset, security(_) <=> true.

activity(decision(X)) :- find_chr_constraint(decision(X)).
activity(task(X)) :- find_chr_constraint(task(X)).
activity(security(X)) :- find_chr_constraint(security(X)).

completed(X), decision(X) <=> true.

ps :-
    find_chr_constraint(Y),
    format('constraint store contains ~w~n', [Y]),
    fail.
ps.

ss :- set_prolog_flag(chr_toplevel_show_store, true).

noss :- set_prolog_flag(chr_toplevel_show_store, false).

:- chr_constraint init/0, completed/2, mumble/0.

% better this way
init ==> task(customize).
init ==> task(help).
init ==> security(csrf_in_forms).
init ==> security(remove_test_reset).
init ==> task(implement_localization_hook).  % customize
init ==> task(set_setting(identity:style)).  % customize
init ==> task(attach_database).
init ==> decision(methods).
init ==> decision(forgot_pw_link).

:- chr_constraint all_email_tasks/0,
    all_remember_me_tasks/0,
    all_public_url_tasks/0.


completed(decision(forgot_pw_link), true) ==> task(customize_forgot_pw_page), all_email_tasks.

% won't work, will simplify away and only do first
all_email_tasks <=> task(attach_email).
all_email_tasks <=> task(set_setting(identity:require_activation_email)).
all_email_tasks <=> all_public_url_tasks.
% keep all_email_tasks, and only add once using
all_email_tasks \ all_email_tasks <=> true.

completed(decision(remember_me), X) ==> X \= none | all_remember_me_tasks.

all_remember_me_tasks <=> task(set_setting(identity:remember_me_duration)).       % customize
all_remember_me_tasks <=> task(set_setting(identity:remember_me_secure)).         % customize


all_public_url_tasks <=> task(set_setting(http:public_host)).
all_public_url_tasks <=> task(set_setting(http:public_port)).
all_public_url_tasks <=> task(set_setting(http:public_scheme)).

%
% when an activity is completed, we remove it

completed(decision(X), _) \ decision(X) <=> true.
completed(task(X), _) \ task(X) <=> true.
completed(security(X), _) \ security(X) <=> true.

%
% name @ retained \ discarded <=> guard | head,body.    Simpagation
% discarded <=> guard | head, body.              Simplification
% retained ==> guard | head, body                Propagation

mumble, mumble, mumble <=> writeln('3 mumbles') | true.
mumble ==> mumble.

doorbell :-
    mumble.



fc(X) :-
    functor(X, XF, N),
    functor(Y, XF, N),
    find_chr_constraint(Y),
    subsumer(X, Y).

subsumer(A, B) :-
    copy_term(B, BCopy)
    , catch(A = B, _, fail)
    , =@=(B, BCopy)
    .


/*
?- fc(my_con(A, 3, B)),
fc(my_con(B, 3, C)).

% above is O(n^2)
:- chr_constraint my_con/3.

% above is linear
:- chr_constraint my_con(+dense_int, +dense_int, +dense_int).

% my_con(A,3,B), my_con(B, 3, C) ==> something.

*/

/*
 *
 *

fc(X) :-
    functor(X, XF, N),
    functor(Y, XF, N),
    find_chr_constraint(Y),
    subsumer(X, Y).

subsumer(A, B) :-
    copy_term(B, BCopy)
    , catch(A = B, _, fail)
    , =@=(B, BCopy)
    .


?- fc(my_con(A, 3, B)),
fc(my_con(B, 3, C)).


:- chr_constraint my_con(+dense_int, +dense_int, +dense_int).

my_con(A,3,B), my_con(B, 3, C) ==> something.

% make all connected edges
a-b b-c c-d
a-d
:- chr_constraint node/2.
node(A,B) \ node(A,B) <=> true.     % anti-cycle

node(A,B), node(B,C) ==> node(A,C).

?- node(1,2), node(2,3), node(3,1).

query(5, 2, X).
:- chr_constraint query(+, +, -).
node(A,D), node(B,E)
\ query(A,B,C)
<=> C is E + D.

% print out execution without pausing
?- chr_leash(-all).  chr_trace.  query(2,3,N).

% better to have init ==> task(customize),task(help),security(csrf_in_forms)...
% even better , since we don't want init
init <=> task(customize),task(help),security(csrf_in_forms)...
% but even better, just do it in prolog
init :- task(customize),task(help),security(csrf_in_forms).
% instead of
init ==> security(remove_test_reset).
init ==> task(implement_localization_hook).  % customize
init ==> task(set_setting(identity:style)).  % customize
init ==> task(attach_database).

?- listing(init).

a \ a <=> true.

% keep all_email_tasks, and only add once using
all_email_tasks \ all_email_tasks <=>

:-chr_constraint  all_email_tasks(+dense_int, +dense_int, +dense_int).

% only do it once pattern
all_email_tasks(    RI,  A,  B)
\ create_all_email(RI ,B)
<=>  true.

create_all_email(RI ,B)
<=>  gen_sym(A), all_email_tasks(    RI,  A,  B).

?- create_all_email(RI ,B), create_all_email(RI ,B).

% this is Ok
completed(decision(remember_me), X) ==> X \= none | all_remember_me_tasks(X).

user(X, UserData)
, completed(decision(remember_me), X) ==> X \= none | all_remember_me_tasks(X).

% put this right at beginning for production
% disable tracing (makes it run faster by  removing debug )
:- chr_option(debug, off).
% let it optimize
:- chr_option(optimize, experimental).

%slow, easy
 :- chr_option(debug, on).  :- chr_option(optimize, off).

 % avoid the constraint store evaporating at top level.
 ?- run_my_program, break.


 */

