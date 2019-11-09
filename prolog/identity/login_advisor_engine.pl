:- module(login_advisor_engine, [
          ps/0,
          ss/0,
          noss/0,
          init/0,
          reset_the_advisor/0,
          num_security/1,
          num_tasks/1,
          num_decisions/1,
          order_remaining_todos/1,
          details/2
          ]).
/** <module> Engine code for the login advisor
 *
 %
% name @ retained \ discarded <=> guard | head,body.    Simpagation
% discarded <=> guard | head, body.              Simplification
% retained ==> guard | head, body                Propagation

 */

:- use_module(library(chr)).

:- chr_constraint
    do/1,
    completed/2,
    reset_advisor/0.

reset_the_advisor :- reset_advisor.

% remove all constraints
reset_advisor \   do(_) <=> true.
reset_advisor \   completed(_, _) <=> true.
reset_advisor <=> true.

completed(X, _) \ do(X) <=> true.

ps :-
    find_chr_constraint(Y),
    format('constraint store contains ~w~n', [Y]),
    fail.
ps.

ss :- set_prolog_flag(chr_toplevel_show_store, true).

noss :- set_prolog_flag(chr_toplevel_show_store, false).

init :-
        do(customize),
        do(csrf_in_forms),
        do(remove_test_reset),
        do(implement_localization_hook),  % customize
        do(set_setting(identity:style)),  % customize
        do(attach_database),
        do(methods),
        do(forgot_pw_link).

:- chr_constraint all_email_tasks/0,
    all_remember_me_tasks/0,
    all_public_url_tasks/0.


completed(forgot_pw_link, true) ==>
      do(customize_forgot_pw_page),
      all_email_tasks.

all_email_tasks <=>
    do(attach_email),
    do(set_setting(identity:require_activation_email)),
    all_public_url_tasks.

completed(remember_me, X) ==> X \= none | all_remember_me_tasks.

all_remember_me_tasks <=>
        do(set_setting(identity:remember_me_duration)), % customize
        do(set_setting(identity:remember_me_secure)).  % customize

all_public_url_tasks <=>
    do(set_setting(http:public_host)),
    do(set_setting(http:public_port)),
    do(set_setting(http:public_scheme)).

		 /*******************************
		 * activity details
		 *******************************/
details(attach_database, _{
                             title: 'Attach Data Store',
                             type: task,
                             order: 0
                         }).
details(customize, _{
                       title: 'Customize Behavior',
                       type: task,
                       order: 10
                   }).
details(csrf_in_forms, _{
                           title: 'Cross Site Resource Forms',
                           type: security,
                           order: 20
                       }).
details(remove_test_reset, _{
                           title: 'Remove test reset',
                           type: security,
                           order: 30
                       }).
details(implement_localization_hook, _{
                                         title: 'Customize/localize Strings',
                                         type: task,
                                         order: 40
                                     }).
details(set_setting(identity:style), _{
                                         title: 'Name of the style to apply to identity pages',
                                         type: task,
                                         order: 50
                                     }).
details(methods, _{
                             title: 'Choose Login Methods',
                             type: decision,
                             order: 70
                         }).
details(forgot_pw_link, _{
                             title: 'Decide about forgot password link',
                             type: decision,
                             order: 80
                         }).
details(attach_email, _{
                           title: 'Attach Email Delivery Service',
                           type: task,
                           order: 90
                       }).
details(set_setting(identity:require_activation_email), _{
                           title: 'Activation Email',
                           type: task,
                           order: 100
                       }).
details(set_setting(identity:remember_me_duration), _{
                           title: 'Set remember me duration',
                           type: task,
                           order: 110
                       }).
details(set_setting(identity:remember_me_secure), _{
                           title: 'Set remember me security',
                           type: task,
                           order: 112
                       }).
details(set_setting(http:public_host), _{
                           title: 'Set the public host name',
                           type: task,
                           order: 120
                       }).
details(set_setting(http:public_port), _{
                           title: 'Set the public host port',
                           type: task,
                           order: 120
                       }).
details(set_setting(http:public_scheme), _{
                           title: 'Set  the public host scheme',
                           type: task,
                           order: 120
                       }).

		 /*******************************
		 * UI support                   *
		 *******************************/

num_security(Sec) :-
    num_of_type(Sec, security).
num_tasks(Tasks) :-
    num_of_type(Tasks, task).
num_decisions(Dec) :-
    num_of_type(Dec, decision).

num_of_type(N, Type) :-
    findall(X, (find_chr_constraint(do(X)),
               details(X, D),
               D.type = Type), List),
    length(List, N).

order_remaining_todos(Tasks) :-
    findall(X, find_chr_constraint(do(X)), List),
    predsort(todo_order, List, Tasks).

todo_order(<, A, B) :-
    details(A, AD),
    details(B, BD),
    AD.order < BD.order.
todo_order(>, A, B) :-
    details(A, AD),
    details(B, BD),
    AD.order > BD.order.
todo_order(=, _, _).
% I know, there's a compare/3 but I'll extend this



