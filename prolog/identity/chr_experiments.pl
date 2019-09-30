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

% cellular automaton from schrijvers slides  slide 82
%
% domain constraint from slides 96 - pattern, guard for instantiation
% to implement constraint solver

% backtracking undoes changes to constraint store
%

% pattern - backtracking for labeling
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

