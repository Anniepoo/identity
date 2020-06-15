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
          retractall_user_property/2,
          current_user_property/1
          ]).

:- use_module(library(http/http_session)).
:- use_module(library(http/html_write)).
:- use_module(library(identity/login_crypto)).
:- use_module(library(identity/customize)).
:- use_module(library(identity/login_email)).

authenticate_user(UName, Password, ok) :-
    user_property(UName, password_hash(Hash)),
    password_hash(Password, Hash), % test requires both ground
    !.
authenticate_user(UName, _, IUOP) :-
    local('Invalid user or password', IUOP), % for security, same as next
    user_property(UName, _),
    !.
authenticate_user(_, _, IUOP) :-
    local('Invalid user or password', IUOP).

% TODO this can fail - probably not handled
% % TODO decide - should it throw?
% TODO - handle attempt to add user when they exist
% gracefully
add_user(UName, Password, Email) :-
    \+ user_property(UName, _),
    password_hash(Password, Hash),
    set_user_property(UName, password_hash(Hash)),
    set_user_property(UName, email(Email)),
    setting(identity:require_activation_email, ActivateEmail),
    (   ActivateEmail = true
    ->  assert_user_property(UName, role(needs_activation)),
        uuid(Key),
        assert_user_property(UName, activation_key(Key)),
        send_activation_email(UName, Email, Key)
    ;   assert_user_property(UName, role(user))
    ).

current_user(UName) :-
    catch(http_session_data(user(UName)),
          _,
          fail).
current_user(guest).

current_user -->
    { current_user(UName) },
    html(UName).

current_user_property(Prop) :-
     current_module(M),
     module_property(M, class(user)),
     \+ memberchk(M, [testcondition,
                      socket,
                      aggregate,
                      prolog_predicate,
                      link_xpce,
                      prolog_colour]),
     current_predicate(M:F/A),
     \+ member(F, [setting]),
     functor(H, F, A),
     catch(clause(H, B), _, fail),
     member(UsesProp, [
                set_user_property(_, Prop),
                assert_user_property(_, Prop),
                retractall_user_property(_, Prop)
            ]),
     sub_term(UsesProp, B),
     \+ var(Prop).




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

