:- module(login_validate, [
          validate_js//2,
          valid/2
          ]).
/** <module> Form validation of username, password, and email
 *
 This module provides both client and server side validation.

 The first argument is a format spec. Format specs are a list of
 options:

  * regex(Regex)
  Regex is a PCRE regular expression in a string.
  * length(Min,Max)
  inclusive range of lengths
  * needs(Type)
  repeatable option, validates only if it has this type
  * forbid(Type)
  repeatable option, validates only if this type absent
  * obscene
  forbids strings with obscene substrings. Note a pengine
  call is made.
  * allow(Type)
  forbid all but this type. If repeated, allow any of the types
  * unique_client
  test uniqueness on client as typed. makes pengine call. implies
  unique
  * unique
  test uniqueness when form submitted. user registration must be
unique usernames so unique is implied.

TODO

Types are char_type/2 types

 */

% re_match("^(?=.{2,8}$).*(?=.{2,8})(?=.*[a-zA-Z])(?=.*\\d)(?=.*[^A-Za-z0-9]).*$",
% "t0!IAok!").


    % /^.*(?=.{2,})(?=.*[a-zA-Z])(?=.*\d)(?=.*[^A-Za-z0-9"]).*$/;
