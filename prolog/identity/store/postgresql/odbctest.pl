:- module(odbctest, [example/0]).

example :-
    % swipl is the name of the ODBC connection in odbc.ini
    odbc_connect('swipl', Connection, []),
    odbc_query(Connection, 'SELECT COUNT(*) FROM todo',COUNT),
    format('posts rows: ~w~n',[COUNT]),
    odbc_query(Connection, 'SELECT COUNT(*) FROM todo',COUNT),
    format('posts rows: ~w~n',[COUNT]),

    odbc_disconnect(Connection).

test_for_tables :-
    % swipl is the name of the ODBC connection in odbc.ini
    odbc_connect('swipl', Connection, []),
    findall(Field, odbc_query(Connection,
 'select table_name, column_name, data_type from information_schema.columns where table_name = \'todo\';',
               Field), Fields),
    permutation(Fields, [row(todo, id, integer), row(todo, descxx, 'character varying')]),
    odbc_disconnect(Connection).
