:- module(login_database, [
          authenticate_user_status/3,
          user_has_role/2]).


authenticate_user_status(annie, _, ok).  % TODO stub for testing
authenticate_user_status(bob, _, 'Bob is a chowderhead').
authenticate_user_status(_, _, 'no idea who that is').

:-multifile identity:special_role/2.

user_has_role(Uname, Role) :-
    once(identity:special_role(Uname, Role)).

