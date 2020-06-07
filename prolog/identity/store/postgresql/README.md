# PostgreSQL  adapter for pack(identity)


## Set up Postgres

Since setting up Postgres can be not nice I am providing detailed instructions.

These taken from a combination of [DigitalOcean instructions](https://www.digitalocean.com/community/tutorials/how-to-install-and-use-postgresql-on-ubuntu-18-04) , [this discussion on the swi-prolog Discourse](https://swi-prolog.discourse.group/t/wiki-discussion-swi-prolog-connecting-to-postgresql-via-odbc/2405/4) , [this repo](https://github.com/roblaing/swipl-webapp-howto/tree/master/unit3) and  [this wiki page on the SWI-Prolog Discourse](https://swi-prolog.discourse.group/t/swi-prolog-connecting-to-postgresql-via-odbc/2404).

May God keep your immortal soul.

----
    sudo apt update
    sudo apt install postgresql postgresql-contrib
    sudo apt-get install unixodbc
    sudo apt-get install odbc-postgresql
----

When you install postgres you will get a new user, **postgres**

Change to this account
----
    sudo -i -u postgres
----


Postgres allows logging into postgres with the same user name as the unix account you're running.


And run

----
   psql
----

Interact with postgres as needed, and then exit.

I made a database named swipl with

    CREATE DATABAE swipl;

You will need a 'role' to operate with. As the postgres user, do

createuser --interactive
I made user **swipl** with password **12345**.
You either need to make this user a superuser, or deal with a bunch of permissions stuff.

And while you're here, make a database. I also called the database **swipl**.

----
    createdb swipl
----

I made a unix account named swipl on my machine.

sudo adduser swipl

and changed to it

sudo -u swipl psql


then you can do 

psql

more conveniently you can just do 

sudo -u sammy psql

Once on psql, you can check your connection info via   `\conninfo`

## ODBC

Your  /etc/odbcinst.ini  file should looks something like

```
groot@galaxy:~$ cat /etc/odbcinst.ini
[PostgreSQL ANSI]
Description=PostgreSQL ODBC driver (ANSI version)
Driver=psqlodbca.so
Setup=libodbcpsqlS.so
Debug=0
CommLog=1
UsageCount=1

[PostgreSQL Unicode]
Description=PostgreSQL ODBC driver (Unicode version)
Driver=psqlodbcw.so
Setup=libodbcpsqlS.so
Debug=0
CommLog=1
UsageCount=1
```

You need to create (if it doesn't exist) /etc/odbc.ini

```
[swipl]
Description = Identity database
Driver      = PostgreSQL ANSI
Servername  = localhost
UserName    = swipl
Password    = 060788
Port        = 5432
Database    = swipl
```


If postgresql service isn't running start it

```
sudo service postgresql start
```


confirm postgres is listening

 ~: netstat -nlp |grep 5432
(Not all processes could be identified, non-owned process info
 will not be shown, you would have to be root to see it all.)
tcp        0      0 127.0.0.1:5432          0.0.0.0:*               LISTEN      -                   
unix  2      [ ACC ]     STREAM     LISTENING     7182349  -                    /var/run/postgresql/.s.PGSQL.5432
 ~: 

Unsure what happened but I had to change swipl's postgres pasword

sudo -u postgres psql

then 
ALTER USER swipl with PASSWORD '12345';
\q

now it's possible from one's normal account to connect via isql, which connects via odbc

isql -v swipl

if this connects you you're good - to make sure things are sane try

 SELECT usename from pg_user;

If you want you can create your tables and such now

CREATE TABLE todo (id INT , descxx VARCHAR(50));


 ctrl-c out











