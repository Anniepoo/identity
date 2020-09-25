:- module(login_persistency, []).
/** <module> Instantiate the library(persistency) based database
 *
 */

:- use_module(library(identity/login_database)).

:- use_module(library(persistency)).

:- persistent
    u_prop(name:atom, prop:acyclic).

login_database:start_db :-
    db_attach('users.db', [sync(flush)]).

%!  user_property(?UName:string, ?Property:acyclic) is nondet
%
%   True when Property is a property of user.
%
login_database:user_property(UName, Property) :-
    with_mutex(login_database,
               bagof(N-P, u_prop(N, P), L)),
    member(UName-Property, L).

%!  set_user_property(+UName:string, +Property:acyclic) is det
%
%   sets the singleton property in database by class,
%   wher class is signature
%
login_database:set_user_property(UName, Property) :-
    Property =.. [PFunctor | Args],
    length(Args, Arity),
    length(Blanks, Arity),
    RetractProperty =.. [PFunctor | Blanks],
    with_mutex(login_database,
               (   retractall_u_prop(UName, RetractProperty),
                   assert_u_prop(UName, Property))).

login_database:assert_user_property(UName, Property) :-
    with_mutex(login_database,
               assert_u_prop(UName, Property)).

login_database:retract_user_property(UName, Property) :-
    with_mutex(login_database,
               retract_u_prop(UName, Property)).

login_database:retractall_user_property(UName, Property) :-
    with_mutex(login_database,
               retractall_u_prop(UName, Property)).

