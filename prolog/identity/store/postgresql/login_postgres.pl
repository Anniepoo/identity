:- module(login_postgres, [database_is_set_up/0, do_setup_database/0]).
/** <module> This module provides a database predicate for PostgreSQL

    see identity/store/postgresql/README.pl to set up the ODBC connection.
    Warning: as this predicates connect to the database, an error from the ODBC
    layer might happen. This will cause an error to be thrown like:
    error(odbc('42P07', 1, 'ERROR: relation "users" already exists;\nError while executing the query'), _4606).
*/

:- use_module(library(settings)).
:- use_module(library(identity/login_database)).

:- setting(identity:odbc_name, atom, swipl, "The name of the odbc connection in odbc.ini").
:- setting(identity:postgres_user_table, atom, users, "User table name in the database").
:- setting(identity:postgres_role_table, atom, roles, "Per-user role table name in the database").
:- setting(identity:postgres_activation_key_table, atom, activationkeys, "Per-user current activation keys table name in the database").
:- setting(identity:postgres_etcetera_table, atom, etcetera, "table for denormalized data").

%! database_is_set_up is semidet.
%
%  succeeds if we can connect to the database and the tables are set-up. If they
%  aren't, you can call do_setup_database/0 to create the right tables.
%
%  @throws if the database can't be connected to
%
database_is_set_up :-
    setting(identity:odbc_name, OdbcName),
    odbc_connect(OdbcName, Connection, []),
    setting(identity:postgres_user_table, UserTableName),
    setting(identity:postgres_role_table, RoleTableName),
    setting(identity:postgres_activation_key_table, ActivationKeyTableName),
    setting(identity:postgres_etcetera_table, EtceteraTableName),
    findall(UserField,
            odbc_query(
                Connection,
                'select table_name, column_name, data_type from information_schema.columns where table_name = \'~w\';'
                        -[UserTableName],
                UserField),
            UserFields),
    permutation(UserFields, [
                    row(UserTableName,id,integer),
                    row(UserTableName,user_name,'character varying'),
                    row(UserTableName,password_hash,'character varying'),
                    row(UserTableName,email,'character varying')]),
    findall(RoleField,
            odbc_query(
                Connection,
                'select table_name, column_name, data_type from information_schema.columns where table_name = \'~w\';'
                        -[RoleTableName],
                RoleField),
            RoleFields),
    permutation(RoleFields, [
                    row(RoleTableName,id,integer),
                    row(RoleTableName,user_id,integer),
                    row(RoleTableName,role,'character varying')]),
    findall(ActivationKeyField,
            odbc_query(
                Connection,
                'select table_name, column_name, data_type from information_schema.columns where table_name = \'~w\';'
                        -[ActivationKeyTableName],
                ActivationKeyField),
            ActivationKeyFields),
    permutation(ActivationKeyFields, [
                    row(ActivationKeyTableName,id,integer),
                    row(ActivationKeyTableName,user_id,integer),
                    row(ActivationKeyTableName,activation_key,'character varying')]),
    findall(EtceteraField,
            odbc_query(
                Connection,
                'select table_name, column_name, data_type from information_schema.columns where table_name = \'~w\';'
                         -[EtceteraTableName],
                EtceteraField),
            EtceteraFields),
    permutation(EtceteraFields, [
                    row(EtceteraTableName,id,integer),
                    row(EtceteraTableName,user_id,integer),
                    row(EtceteraTableName,functor,'character varying'),
                    row(EtceteraTableName,prop,text)]),
    odbc_disconnect(Connection).

%! do_setup_database is det.
%
%  do_setup_database will setup the tables for users, roles, and activation
%  keys, using as table names the settings identity:postgres_user_table,
%  identity:postgres_role_table and identity:postgres_activation_key_table.
%  @throws errors if the database is not reacheable, or the tables are already
%  existing.
do_setup_database :-
    setting(identity:odbc_name, OdbcName),
    odbc_connect(OdbcName, Connection, []),
    setting(identity:postgres_user_table, UserTableName),
    setting(identity:postgres_role_table, RoleTableName),
    setting(identity:postgres_activation_key_table, ActivationKeyTableName),
    setting(identity:postgres_etcetera_table, EtceteraTableName),
    odbc_query(Connection, 'CREATE TABLE ~w (\c
      id SERIAL PRIMARY KEY,\c
      user_name varchar(256),\c
      password_hash varchar(256),\c
      email varchar(256))' -[UserTableName]),
    odbc_query(Connection, 'CREATE TABLE ~w (\c
      id SERIAL PRIMARY KEY,\c
      user_id int,\c
      role varchar(256),\c
      FOREIGN KEY (user_id) REFERENCES ~w(id))' -[RoleTableName, UserTableName]),
    odbc_query(Connection, 'CREATE TABLE ~w (
      id SERIAL PRIMARY KEY,\c
      user_id int,\c
      activation_key varchar(256),\c
      FOREIGN KEY (user_id) REFERENCES ~w(id))' -[ActivationKeyTableName, UserTableName]),
    odbc_query(Connection, 'CREATE TABLE ~w (
      id SERIAL PRIMARY KEY,\c
      user_id int,\c
      functor varchar(256),\c
      prop text,\c
      FOREIGN KEY (user_id) REFERENCES ~w(id))' -[EtceteraTableName, UserTableName]),
    odbc_disconnect(Connection).

login_database:start_db :-
    database_is_set_up.

%!  user_property(?UName, ?Property) is nondet
%
%   True when Property is a property of user.
%
login_database:user_property(UName, Property) :-
    with_mutex(
        login_database,
        setup_call_cleanup(
            (
                setting(identity:odbc_name, OdbcName),
                odbc_connect(OdbcName, Connection, [])
            ),
            (
                user_property_(Connection, UName, Property)
            ),
            (
                odbc_disconnect(Connection)
            )
        )
    ).

:- multifile
    login_use_user_sql/1,  % test property to see if user supplied sql
    login_user_sql/3.   % user supplied sql getter


%!  user_passwordHash(?UName, -PasswordHash) is nondet
%
%   True when Property is a property of user.
%
user_property_(Connection, UName, password_hash(PasswordHash)) :-
    setting(identity:postgres_user_table, UserTableName),
    odbc_query(
        Connection,
        'SELECT password_hash from ~w WHERE user_name = \'~w\'' -[UserTableName, UName],
        row(PasswordHash)
    ).
user_property_(Connection, UName, email(Email)) :-
    setting(identity:postgres_user_table, UserTableName),
    odbc_query(
        Connection,
        'SELECT email from ~w WHERE user_name = \'~w\'' -[UserTableName, UName],
        row(Email)
    ).
user_property_(Connection, UName, role(Role)) :-
    setting(identity:postgres_user_table, UserTableName),
    setting(identity:postgres_role_table, RoleTableName),
    odbc_query(
        Connection,
        'SELECT role from ~w JOIN ~w ON ~w.id = ~w.user_id WHERE ~w.name = \'~w\''-[
                          RoleTableName,
                          UserTableName,
                          UserTableName,
                          RoleTableName,
                          UserTableName,
                          UName],
    row(Role)
    ).
user_property_(Connection, UName, activation_key(ActivationKey)) :-
    setting(identity:postgres_user_table, UserTableName),
    setting(identity:postgres_activation_key_table, ActivationKeyTableName),
    odbc_query(
        Connection,
        'SELECT activation_key from ~w JOIN ~w ON ~w.id = ~w.user_id WHERE  ~w.name = \'~w\'' -
             [ActivationKeyTableName,
              UserTableName,
              UserTableName,
              ActivationKeyTableName,
              UserTableName,
              UName],
        row(ActivationKey)
    ).
user_property_(Connection, UName, Property) :-
    \+ member(Property, [password_hash(_), email(_), role(_), activation_key(_)]),
    login_use_user_sql(Property),
    !,
    login_user_sql(Connection, UName, Property).
user_property_(Connection, UName, Property) :-
    setting(identity:postgres_user_table, UserTableName),
    setting(identity:postgres_etcetera_table, EtceteraTableName),
    odbc_query(
        Connection,
        'SELECT prop from ~w JOIN ~w ON ~w.id = ~w.user_id WHERE  ~w.name = \'~w\'' -
             [EtceteraTableName,
              UserTableName,
              UserTableName,
              EtceteraTableName,
              UserTableName,
              UName],
        row(PropString)
    ),
    text_to_string(PropString, PropS),
    term_string(Property, PropS).

%!  set_user_property(+UName:string, +Property:acyclic) is det
%
%   sets the singleton property in database by class,
%   where class is signature
%

%!  assert_user_property(+UName:string, +Property:acyclic) is det
%
%   adds a property
%


%!  retract_user_property(+UName:string, +Property:acyclic) is det
%
%   removes a single instance of a property
%   (normally not what you want, use retractall unless you're using
%   multiset property)
%

%!  retract_user_property(+UName:string, +Property:acyclic) is det
%
%   removes a single instance of a property
%   (normally not what you want, use retractall unless you're using
%   multiset property)
%


%!  retractall_user_property(+UName:string, +Property:acyclic) is det
%
%   removes all instances of a property
%


%!  start_db_expansion is det
%
%   call at startup to give the storage method a chance to
%   start up. May fail or throw if the db isn't able to start up
%
