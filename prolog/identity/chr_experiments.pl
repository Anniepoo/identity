/*
 *  CHR syntax, since I can never remember it:
 *  name @ retained \ discarded <=> guard | head,body.    Simpagation
 *  name @ discarded <=> guard | head, body.      Simplification
 *  name @ retained ==> guard | head, body.         Propagation
 *
*/
:- use_module(library(chr)).


:- chr_constraint
    foo/1,
    bar/1,
    mep/1,
    zap/1.

foo(X) ==> member(X, [a,b,c]), bar(X).

% doesn't work
% mep(L) ==> member(X, L), zap(X).

mep(L) ==> foreach(member(X, L), zap(X)).

% how do I negate?
:- chr_constraint nord/0, noodge/0, dingle/0.

nord ==> \+ find_chr_constraint(noodge) | dingle.

% Examples to write
%
% cellular automaton from schrijvers slides  slide 82
%
%domain constraint from slides 96
%
%
% toy project - recipe reasoner
%  (use drinks)

% toy project - radioactive decay
% given half lives, put in atoms and watch'em decay
% keeping track of time

% adventure game.


%patterns
% domain constraint from slides 96 - pattern, guard for instantiation
% to implement constraint solver
%

% backtracking undoes changes to constraint store
%

% pattern - backtracking for labeling
%

% pattern - get_gcd is a chr_constraint used only
% to get the value of another constraint.
% Not sure if this backtracks
%
% Note that the binding happens in the BODY, cause binding in the
% head is a nono
%
% constraint to get the current gcd/1 value
% gcd(N) \ get_gcd(M) <=> M = N.
%



		 /*******************************
		 *     helpful utilities        *
		 *******************************/

% print out the constraint store
ps :-
    find_chr_constraint(Y),
    format('constraint store contains ~w~n', [Y]),
    fail.
ps.

% print out constraint store when you return to top level
ss :- set_prolog_flag(chr_toplevel_show_store, true).

% or don't
noss :- set_prolog_flag(chr_toplevel_show_store, false).



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

% saner way to do same
?- set_prolog_flag(toplevel_mode, recursive).

 */

