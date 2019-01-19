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

		 /*******************************
		 *       Localization		*
		 *******************************/

:- html_meta local(+, ?, ?).
local(English) -->
    { local(English, Local) },
    html(Local).

local(X, X).


		 /*******************************
		 *           SETTINGS           *
		 *******************************/
