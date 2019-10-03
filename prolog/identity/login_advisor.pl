:- module(login_advisor, [
              run_advisor/0
          ]).
/** <module> Advisor for assisting with setup
 *
 * Note that this module does NOT load automatically.
 *
 * To run the advisor, load_module(library(identity/login_advisor)).
 * Then query  run_advisor.
 *
 */

:- use_module(library(identity/login_advisor_engine)).

run_advisor :-
    advisor_help,
    reset_the_advisor,  % for the moment
    init,
    advisor_loop.

advisor_loop :-
    show_status,
    set_prompt,
    read(Input),
    (   process_cmd(Input),
        advisor_loop
    ;
        true
    ).

show_status :-
    nl,
    writeln('TODO show status').
set_prompt :-
    num_security(Sec),
    num_tasks(Tasks),
    num_decisions(Dec),
    format(atom(P), '~w/~w/~w >', [Sec, Tasks, Dec]),
    prompt(_, ' >'),  % so we get continuation lines
    prompt1(P).

advisor_help :-
    writeln('======= pack(identity) setup advisor ========'),
    nl,
    writeln('This advisor will help you step through everything'),
    writeln('you need to do to create a complete user login and'),
    writeln('registration system.'),
    nl,
    advisor_cmd_help.

advisor_cmd_help :-
    writeln('all commands are period terminated prolog terms.'),
    writeln('stop - exit the process (saving TBD)'),
    writeln('show - show the remaining tasks'),
    writeln('help - show the commands').

process_cmd(stop) :- !,fail.
process_cmd(help) :- !, advisor_cmd_help.
process_cmd(show) :-
    order_remaining_todos(Tasks),
    show_task_list(1, Tasks).
process_cmd(X) :-
    format('do cmd ~w~n', [X]).

show_task_list(_, []).
show_task_list(N, [H | T]) :-
    details(H, D),
    _{
        type: security,
        title: Title
    }   :< D,
    format('~w) Security Issue: ~w~n', [N, Title]),
    succ(N, NN),
    show_task_list(NN, T).
show_task_list(N, [H | T]) :-
    details(H, D),
    _{
        type: task,
        title: Title
    }   :< D,
    format('~w) Task: ~w~n', [N, Title]),
    succ(N, NN),
    show_task_list(NN, T).
show_task_list(N, [H | T]) :-
    details(H, D),
    _{
        type: decision,
        title: Title
    }   :< D,
    format('~w) Decision: ~w~n', [N, Title]),
    succ(N, NN),
    show_task_list(NN, T).
show_task_list(N, [H | T]) :-
    gtrace,
    format('~w) ~w not parsed~n', [-N, H]),
    succ(N, NN),
    show_task_list(NN, T).
