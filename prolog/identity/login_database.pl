:- module(login_database, [
          authenticate_user/3,
          add_user/3,
          current_user/1,
          current_user//0,
          user_property/2,
          set_user_property/2,
          assert_user_property/2,
          retract_user_property/2,
          retractall_user_property/2,
          current_user_property/1,
          start_db/0
          ]).
/** <module> Abstraction layer for database connection
 *
 * The 'default' persistency based store used to live here. Moved to
 * store/persistency/login_persistency.pl
 */

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
    user_property/2,
    set_user_property/2,
    assert_user_property/2,
    retract_user_property/2,
    retractall_user_property/2,
    start_db/0.

%!  user_property(?UName, ?Property) is nondet
%
%   True when Property is a property of user. In a real application this
%   should of course be  a  proper   persistent  database  and passwords
%   should be properly hashed.
%

%!  set_user_property(+UName:string, +Property:acyclic) is det
%
%   sets the singleton property in database by class,
%   where class is signature
%

%!  assert_user_property(+UName:string, +Property:acyclic) is det
%
%   adds a property
%


%!  retract_user_property(+UName:string, +Property:acyclic) is det
%
%   removes a single instance of a property
%   (normally not what you want, use retractall unless you're using
%   multiset property)
%

%!  retract_user_property(+UName:string, +Property:acyclic) is det
%
%   removes a single instance of a property
%   (normally not what you want, use retractall unless you're using
%   multiset property)
%


%!  retractall_user_property(+UName:string, +Property:acyclic) is det
%
%   removes all instances of a property
%


%!  start_db_expansion is det
%
%   call at startup to give the storage method a chance to
%   start up. May fail or throw if the db isn't able to start up
%









