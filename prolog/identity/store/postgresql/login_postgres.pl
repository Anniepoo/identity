:- module(login_postgres, [database_is_set_up/0, do_setup_database/0]).
/** <module> This module provides a database predicate for PostgreSQL

    see identity/store/postgresql/README.pl to set up the ODBC connection.
    Warning: as this predicates connect to the database, an error from the ODBC
    layer might happen. This will cause an error to be thrown like:
    error(odbc('42P07', 1, 'ERROR: relation "users" already exists;\nError while executing the query'), _4606).
*/

:- use_module(library(settings)).

:- setting(identity:odbc_name, atom, swipl, "The name of the odbc connection in odbc.ini").
:- setting(identity:postgres_user_table, atom, users, "User table name in the database").
:- setting(identity:postgres_role_table, atom, roles, "Per-user role table name in the database").
:- setting(identity:postgres_activation_key_table, atom, activationkeys, "Per-user current activation keys table name in the database").

%! database_is_set_up is semidet.
%
%  succeeds if we can connect to the database and the tables are set-up. If they
%  aren't, you can call do_setup_database/0 to create the right tables.
database_is_set_up :-
    setting(identity:odbc_name, OdbcName),
    odbc_connect(OdbcName, Connection, []),
    setting(identity:postgres_user_table, UserTableName),
    setting(identity:postgres_role_table, RoleTableName),
    setting(identity:postgres_activation_key_table, ActivationKeyTableName),
    findall(UserField,
            odbc_query(
                Connection,
                'select table_name, column_name, data_type from information_schema.columns where table_name = \'~w\';' -[UserTableName],
                UserField),
            UserFields),
    permutation(UserFields, [
                    row(users,id,integer),
                    row(users,user_name,'character varying'),
                    row(users,password_hash,'character varying'),
                    row(users,email,'character varying')]),
    findall(RoleField,
            odbc_query(
                Connection,
                'select table_name, column_name, data_type from information_schema.columns where table_name = \'~w\';' -[RoleTableName],
                RoleField),
            RoleFields),
    permutation(RoleFields, [
                    row(roles,id,integer),
                    row(roles,user_id,integer),
                    row(roles,role,'character varying')]),
    findall(ActivationKeyField,
            odbc_query(
                Connection,
                'select table_name, column_name, data_type from information_schema.columns where table_name = \'~w\';' -[ActivationKeyTableName],
                ActivationKeyField),
            ActivationKeyFields),
    permutation(ActivationKeyFields, [
                    row(activationkeys,id,integer),
                    row(activationkeys,user_id,integer),
                    row(activationkeys,activation_key,'character varying')]),
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
    odbc_disconnect(Connection).
