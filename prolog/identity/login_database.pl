:- module(login_database, [
          use_default_db/0,
          user_property/2,
          set_user_property/2,
          assert_user_property/2,
          retract_user_property/2,
          retractall_user_property/2
          ]).

		 /*******************************
		 *            USER DATA		*
		 *******************************/

:- multifile
    user_property_expansion/2,
    set_user_property_expansion/2,
    assert_user_property_expansion/2,
    retract_user_property_expansion/2,
    retractall_user_property_expansion/2.

:- dynamic using_default_db/0.

:- use_module(library(persistency)).

:- persistent
    u_prop(name:atom, prop:acyclic).

use_default_db :-
    db_attach('users.db', [sync(flush)]),
    asserta(using_default_db).

%!  user_property(?UName, ?Property) is nondet
%
%   True when Property is a property of user. In a real application this
%   should of course be  a  proper   persistent  database  and passwords
%   should be properly hashed.
%
user_property(UName, Property) :-
    user_property_expansion(UName, Property).
user_property(UName, Property) :-
    using_default_db,
    with_mutex(login_database,
               bagof(N-P, u_prop(N, P), L)),
    member(UName-Property, L).

set_user_property(UName, Property) :-
    set_user_property_expansion(UName, Property).
set_user_property(UName, Property) :-
    using_default_db,
    Property =.. [PFunctor | Args],
    length(Args, Arity),
    length(Blanks, Arity),
    RetractProperty =.. [PFunctor | Blanks],
    with_mutex(login_database,
               (   retractall_u_prop(UName, RetractProperty),
                   assert_u_prop(UName, Property))).

assert_user_property(UName, Property) :-
    assert_user_property_expansion(UName, Property).
assert_user_property(UName, Property) :-
    using_default_db,
    with_mutex(login_database,
               assert_u_prop(UName, Property)).

retract_user_property(UName, Property) :-
    retract_user_property_expansion(UName, Property).
retract_user_property(UName, Property) :-
    using_default_db,
    with_mutex(login_database,
               retract_u_prop(UName, Property)).

retractall_user_property(UName, Property) :-
    retractall_user_property_expansion(UName, Property).
retractall_user_property(UName, Property) :-
    using_default_db,
    with_mutex(login_database,
               retractall_u_prop(UName, Property)).

