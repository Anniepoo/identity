:- module(login_database, [
          authenticate_user/3,
          add_user/3,
          current_user/1,
          current_user//0,
          use_default_db/0,
          user_property/2,
          set_user_property/2,
          assert_user_property/2,
          retract_user_property/2,
          retractall_user_property/2
          ]).
% TODO document all 'known' properties
%
:- use_module(library(http/http_session)).
:- use_module(library(http/html_write)).
:- use_module(library(identity/login_crypto)).

authenticate_user(UName, Password, ok) :-
    user_property(UName, password_hash(Hash)),
    password_hash(Password, Hash), % test requires both ground
    !.
authenticate_user(UName, _, 'Invalid Password') :-
    user_property(UName, _),
    !.
authenticate_user(_, _, 'No Such User').

% TODO this can fail - probably not handled
% % TODO decide - should it throw?
add_user(UName, Password, Email) :-
    \+ user_property(UName, _),
    password_hash(Password, Hash),
    set_user_property(UName, password_hash(Hash)),
    set_user_property(UName, email(Email)),
    assert_user_property(UName, role(user)).

current_user(UName) :-
    http_session_data(user(UName)).
current_user(guest).

current_user -->
    { current_user(UName) },
    html(UName).


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
               u_prop(UName, Property)).

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

