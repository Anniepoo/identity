:- module(login_database, [
          authenticate_user_status/3,
          user_has_role/2,
          add_user/3,
          current_user/1,
          current_user//0,
          user_property/2
          ]).

:- use_module(library(http/http_session)).
:- use_module(library(http/html_write)).

:- dynamic user/3.

% TODO this is redundant, use user_property
authenticate_user_status(annie, _, ok).  % TODO stub for testing
authenticate_user_status(bob, _, 'Bob is a chowderhead').
authenticate_user_status(UName, Password, ok) :-
    user(UName, Password, _).
authenticate_user_status(_, _, 'no idea who that is').

:-multifile identity:special_role/2.

user_has_role(Uname, Role) :-
    once(identity:special_role(Uname, Role)).

add_user(UName, Password, Email) :-
    asserta(user(UName, Password, Email)).

current_user(UName) :-
    http_session_data(user(UName)).
current_user(guest).

current_user -->
    { current_user(UName) },
    html(UName).


		 /*******************************
		 *            USER DATA		*
		 *******************************/


%!  user_property(?User, ?Property) is nondet.
%
%   True when Property is a property of user. In a real application this
%   should of course be  a  proper   persistent  database  and passwords
%   should be properly hashed.

user_property(jan, password(geheim)).
user_property(jan, role(trusted)).
user_property(_, role(user)).   % everyone logged in is a user
user_property(bob, password(secret)).
