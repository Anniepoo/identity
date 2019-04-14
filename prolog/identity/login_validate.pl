:- module(login_validate, [
          validate_js//0,
          valid/2
          ]).
/** <module> Form validation of username, password, and email
 *
 This module provides both client and server side validation.

 */

:- use_module(library(http/html_write)).
:- use_module(library(http/js_write)).
:- use_module(library(pcre)).
:- use_module(library(identity/customize)).

:- setting(identity:constraints,
           dict,
    _{
        email: _{ min: 4,
                  max: 128,
                  regex: '^[A-Za-z0-9\\-_\\+\\.]+@(([A-Za-z0-9\\-_\\+]+)\\.)+[A-Za-z0-9]+$',
                  warn: 'Must be a valid email address'
                },
        uname: _{ min: 4,
                  max: 128,
                  regex: '^[A-Za-z0-9\\-_\\+\\.]+$',
                  warn: 'User name must be 4-128 characters from a-z, A-Z, 0-9, - and _'
                },
        passwd: _{ min: 4,
                   max: 999,
                   regex: '^(?=.{8,999}$)(?=.*[a-z])(?=.*[A-Z])(?=.*\\d)(?=.*[^A-Za-z0-9]).*$',
                   warn:  'Password must be at least 8 long, and contain a capital letter, a lowercase letter, a digit, and a special symbol like !@#$%^&*()'
                 },
        passwd2: _{
                     min: 4,
                     max: 999,
                     regex: '^(?=.{8,999}$)(?=.*[a-z])(?=.*[A-Z])(?=.*\\d)(?=.*[^A-Za-z0-9]).*$',
                     warn: 'Field below must match password'
                 }
    },
           "A dict with the constraints to apply to the registration form"
).

constraints(X) :-
    setting(identity:constraints, X).

% You're a wonderful bit of javascript. You're completely
% valid, and perfect just as you are.
% I feel your pain.
%
validate_js -->
    { constraints(Constraints) },
    html([\js_script({|javascript(Constraints)||
         const loginConstraints = Constraints;

         const loginTimers = {
                   email: null,
                   passwd: null,
                   uname: null,
                   passwd2: null
               };

         document.getElementById("emailwarn").innerHTML =
                      loginConstraints['email'].warn;
         document.getElementById("unamewarn").innerHTML =
                      loginConstraints['uname'].warn;
         document.getElementById("passwdwarn").innerHTML =
                      loginConstraints['passwd'].warn;
         document.getElementById("passwd2warn").innerHTML =
                      loginConstraints['passwd2'].warn;

         function validateIdentity(Element) {
                      loginTimers[Element.name] = null;
                      console.log(Element.value);
                      console.log(Element.name);
                      var c = loginConstraints[Element.name];

                      if( Element.name == "passwd2") {
                          var pw =  document.getElementById("passwd").value;
                          var pw2 = Element.value;
                          console.log(pw);
                          console.log(pw2);

                          if(pw === pw2) {
                              Element.classList.remove("error");
                              document.getElementById(Element.name + "warn").classList.remove('warn');
                          } else {
                              Element.classList.add("error");
                              document.getElementById(Element.name + "warn").classList.add('warn');
                            }
                          return;
                      }

                      var patt = new RegExp(c.regex);

                      if(Element.value.length < c.min ||
                         Element.value.length > c.max ||
                         patt.exec(Element.value) == null
                         ) {
                          Element.classList.add("error");
                          document.getElementById(Element.name + "warn").classList.add('warn');
                      } else {
                          Element.classList.remove("error");
                          document.getElementById(Element.name + "warn").classList.remove('warn');
                      }
                  }

           function doValidation(Element) {
               if(loginTimers[Element.name] != null) {
                   clearTimeout(loginTimers[Element.name]);
               }
               loginTimers[Element.name] = setTimeout(
                                               () => validateIdentity(Element),
                                           600);
           }
         |}),
        style('.error { border: 3px solid #FF0000; }\n.warning { display: none; }\n.warning.warn { display: block;\ncolor: #aa6666; }')
         ]).
% TODO get message form from local
valid(FieldName=Value, Status) :-
    constraints(C),
    string_length(Value, L),
    C.FieldName.min > L,
    !,
    local('~w is too short, must be at least ~w~n', Fmt),
    format(atom(Status), Fmt,
           [FieldName, C.FieldName.min]).
valid(FieldName=Value, Status) :-
    constraints(C),
    string_length(Value, L),
    C.FieldName.max < L,
    !,
    local('~w is too long, must be at most ~w~n', Fmt),
    format(atom(Status), Fmt,
           [FieldName, C.FieldName.max]).
valid(FieldName=Value, Status) :-
    constraints(C),
    \+ re_match(C.FieldName.regex, Value),
    !,
    format(atom(Status), '~w~n', [C.FieldName.warn]).
valid(_=_, ok).

% TODO handle passwd2 special case
/*
% TODO table this
%
:- meta_predicate if_opt(+, 3, +, ?, ?).

if_opt(Pattern, DCG, Options, A, B) :-
    memberchk(Pattern, Options),
    call(DCG, Options, A, B).
if_opt(Pattern, _, Options) -->
    { \+ memberchk(Pattern, Options) },
    [].


pcre_regex(Options) -->
    "^",
    if_opt(regex(_), regex_section, Options),
    if_opt(length(_,_), length_section, Options),
    if_opt(forbid(_), forbid_sections, Options),
    if_opt(allow(_), allow_sections, Options),
    ".*",
    if_opt(needs(_), needs_sections, Options),
    ".*$".

regex_section(Options) -->
    { bagof(Regex, member(regex(Regex), Options), Regexes) },
    regex_patt(Regexes).

regex_patt([]) --> [].
regex_patt([Patt|Rest]) -->
    "(?=",
    { string_codes(Patt, Codes) },
    Codes,
    "$)",
    regex_patt(Rest).

length_section(Options) -->
    { memberchk(length(Min, Max), Options),
      format(codes(Codes), "(?=.{~d,~d}$)", [Min, Max])
    },
    Codes.

forbid_section(Options) -->
    { bagof(Type, member(forbid(Type), Options), Types) },
    forbid_patt(Types).

forbid_patt([]) --> [].
forbid_patt([Type|Rest]) -->
    "(?=[^",
    type_pcre(Type),
    "]*$)",
    forbid_patt(Rest).

type_pcre(Type) -->
    { type_pcre(Type, Codes) },
    Codes.

type_pcre(alnum, `0-9a-zA-Z`).
type_pcre(alpha, `a-zA-Z`).
type_pcre(csym, `_0-9a-zA-Z`).
type_pcre(csymf, `_a-zA-Z`).
type_pcre(white, `\\t `).

% TODO partly done
%
% TODO not working - some types can't be converted.


*/






