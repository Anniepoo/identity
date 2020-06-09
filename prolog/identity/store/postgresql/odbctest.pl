:- module(odbctest, [example/0]).

example :-
    % swipl is the name of the ODBC connection in odbc.ini
    odbc_connect('swipl', Connection, []),
    odbc_query(Connection, 'SELECT COUNT(*) FROM todo',COUNT),
    format('posts rows: ~w~n',[COUNT]),
    odbc_query(Connection, 'SELECT COUNT(*) FROM todo',COUNT),
    format('posts rows: ~w~n',[COUNT]),

    odbc_disconnect(Connection).
