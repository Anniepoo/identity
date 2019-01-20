:- module(customize, [
 %             localized//1,
 %             localized/2,
              local//1,
              local/2
          ]).
/** <module> User settings and config for identity
 *
 */

:- use_module(library(http/html_write)).
:- use_module(library(settings)).

		 /*******************************
		 *       Localization		*
		 *******************************/

:- html_meta local(+, ?, ?).
local(English) -->
    { local(English, Local) },
    html(Local).

:- multifile customize:local_hook/2.

local(X, Y) :-
    customize:local_hook(X,Y),
    !.
local(X, X).


		 /*******************************
		 *           SETTINGS           *
		 *******************************/
:- setting(identity:style, atom, default,
           "Name of the style to apply to identity pages").
:- setting(identity:require_activation_email, boolean, false,
           "true = require activation email, false = not required").
