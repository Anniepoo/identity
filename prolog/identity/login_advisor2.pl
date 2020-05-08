:- module(login_advisor2, [login_advisor2/0]).
/** <module> Login Advisor rev 2
 *
 * This is an expert system like tool to help set up
 * an identity instance
 */
:- dynamic question_answer/2.

login_advisor2 :-
    retractall(question_answer(_, _)),  % TODO expand
    gather_options(Options),
    implement_options(Options).

implement_options(Options) :-
    writeq(Options), nl.   % TODO not doing this today

gather_options(Options) :-
    phrase(options, Options).


options -->
    ensure_identity_loaded,
    get_default_db.


ensure_identity_loaded -->
    question(is_identity_loaded,
             "is identity being loaded?",
             ["Yes", "No"],
             Answer),
    [is_identity_loaded(Answer)].

get_default_db -->
    question(use_default_db,
             "Should we use the default, persistance based database?",
             ["Yes", "No"],
             Answer),
    configure_db(Answer).

configure_db("Yes") -->
    [db_configuration(default)].
configure_db("No") -->
    question(use_available_db_type,
             "Do you want to use one of these DB solutions? Mariadb, Neo4j",
             ["Yes", "No"],
             Answer),
    configure_non_default_db(Answer).

configure_non_default_db("Yes") -->
    question(non_default_db_type,
             "Which DB should we use?",
             ["Mariadb", "Neo4j"],
            Answer),
    configure_specific_db(Answer).
% need the "No" answer

configure_specific_db("Mariadb") -->
    open_question("What is the DSN for your Database?", Answer),
    [db_configuration(db_connection(mariadb, Answer))].
% need the Neo4j answer



question(Name, _, _, Answer) -->
    [],
    {  question_answer(Name, Answer), !}.
question(Name,
         Prompt,
         Choices,
         Answer) -->
    [],
    {
        format('~w~n', [Prompt]),
        display_choices(Choices),
        get_choice(Choice),
        nth0(Choice, Choices, Answer),
        retractall(question_answer(Name, _)),
        asserta(question_answer(Name, Choice))
    }.

display_choices(Choices) -->
    [],
    {

    }








