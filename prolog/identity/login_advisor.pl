:- module(login_advisor, [
              run_advisor/0
          ]).
/** <module> Advisor for assisting with setup
 *
 * Note that this module does NOT load automatically.
 *
 * To run the advisor, load_module(library(identity/login_advisor)).
 * Then query  run_advisor.
 *
 */

:- use_module(library(http/http_path)).
:- use_module(library(http/html_write)).
:- use_module(library(http/html_head)).
:- use_module(library(http/http_dispatch)).
:- use_module(library(http/http_parameters)).

:- dynamic adviser_running/0.

/*
:- html_resource(jquery, [virtual(true), requires('https://stackpath.bootstrapcdn.com/bootstrap/4.3.1/js/bootstrap.min.js'), mime_type('text/javascript')]).
:- html_resource(popper, [virtual(true), requires('https://cdnjs.cloudflare.com/ajax/libs/popper.js/1.14.7/umd/popper.min.js'), mime_type('text/javascript')]).
:- html_resource(bootstrap, [
            requires([jquery, popper,
    'https://stackpath.bootstrapcdn.com/bootstrap/4.3.1/css/bootstrap.min.css']),
                             ordered(true),
                             virtual(true), mime_type('text/css')]).
*/

run_advisor :-
    asserta(adviser_running),
    http_absolute_location(login(advisor), URL, []),
    www_open_url(URL).

:- http_handler(login(advisor), advisor, [id(advisor)]).

advisor(_Request) :-
    \+ adviser_running,
    reply_html_page(
        title('pack(identity) Advisor'),
        \no_advisor_page).
advisor(Request) :-
    adviser_running,
    http_parameters(Request,
                    [
                        page(Page, [integer, default(0)])
                    ]),
    all_decisions(Decisions),
    all_tasks(Tasks),
    all_security(Security),
    reply_html_page(
        \advisor_head,
        \advisor_page(_{
                      page: Page,
                      decisions: Decisions,
                      tasks: Tasks,
                      security: Security
                      })
    ).

no_advisor_page -->
    html([h1('Need to start the advisor'),
          p('for security, you need to start the advisor from the console.'),
          code(['query  ', b(class(prompt), ['?-', &(nbsp)]),
                b(class(code), 'run_advisor.')])
         ]).

advisor_head -->
    html([title('pack(identity) Advisor')
         /* bootstrap needs this,
      meta([name(viewport),
            content('width=device-width, initial-scale=1, shrink-to-fit=no')
           ]) */
         ]).

advisor_page(Args) -->
    { nth0(Args.page(), Args.decisions(), CurDecision) },
    html([
        \advisor_style,
        h1('pack(identity) Setup Advisor'),
          div(class(container), [
                  \decision_list(Args.decisions(), Args.page),
                  \decision_area(CurDecision),
                  div(class('right-sidebar'), [
                        \task_list(Args.tasks()),
                        \security_list(Args.security())
                      ])
              ])
    ]).

decision_list(Decisions, Page) -->
    html(div(class(decisions),
             [
                h2(
title('Decisions you need to make - &#xFE0F items are completed'),
                    'Decisions'),
                div(\decision_list_contents(Decisions, Page))
             ])).

decision_list_contents([], _) --> [].
decision_list_contents([H|T], 0) -->
    html(\decision_item(selected, H)),
    decision_list_contents(T, -1).
decision_list_contents([H|T], N) -->
    { N \= 0,
      NN is N - 1
    },
    html(\decision_item(unselected, H)),
    decision_list_contents(T, NN).

decision_item(Selected, Decision) -->
    html([div(class(['decision-item', Selected]),
              [
              \decision_sigil(Decision.status()),
              span(title(Decision.rollover()), Decision.title())
              ]
          )]).

decision_sigil(decided) -->
    html(span(&#(0x1f601))). % beaming face emoji
decision_sigil(undecided) -->
    html(span(&#(0x1f914))). % thinking face emoji

% &#(0xFE0F)  is green check
%


advisor_style -->
    html({|html||
     <style>
html {
  box-sizing: border-box;
}
*, *:before, *:after {
  box-sizing: inherit;
}
     </style>
          |}).









