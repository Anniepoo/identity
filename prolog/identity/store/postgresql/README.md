# PostgreSQL  adapter for pack(identity)


## Set up Postgres

Since setting up Postgres can be not nice I am providing detailed instructions.

These taken from a combination of [DigitalOcean instructions](https://www.digitalocean.com/community/tutorials/how-to-install-and-use-postgresql-on-ubuntu-18-04) , [this discussion on the swi-prolog Discourse](https://swi-prolog.discourse.group/t/wiki-discussion-swi-prolog-connecting-to-postgresql-via-odbc/2405/4) , [this repo](https://github.com/roblaing/swipl-webapp-howto/tree/master/unit3) and  [this wiki page on the SWI-Prolog Discourse](https://swi-prolog.discourse.group/t/swi-prolog-connecting-to-postgresql-via-odbc/2404).

May God keep your immortal soul.

    sudo apt update
    sudo apt install postgresql postgresql-contrib
    sudo apt-get install unixodbc
    sudo apt-get install odbc-postgresql

When you install postgres you will get a new user, **postgres**

Change to this account

    sudo -i -u postgres


Postgres allows logging into postgres with the same user name as the unix account you're running.


And run

   psql

Interact with postgres as needed, and then exit.

I made a database named swipl with

    CREATE DATABAE swipl;

You will need a 'role' to operate with. As the postgres user, do

createuser --interactive
I made user **swipl** with password **12345**.
You either need to make this user a superuser, or deal with a bunch of permissions stuff.

And while you're here, make a database. I also called the database **swipl**.

    createdb swipl

I made a unix account named swipl on my machine.

	sudo adduser swipl

and changed to it

	sudo -u swipl psql


then you can do 

	psql

more conveniently you can just do 

	sudo -u swipl psql

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
Driver      = PostgreSQL Unicode
Servername  = localhost
UserName    = swipl
Password    = 12345
Port        = 5432
Database    = swipl
```

(the password is, I believe, the password of the linux account)

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

now it's possible from one's normal account to test the ODBC connecton.

From your normal account, connect via isql, which connects via odbc

	isql -v swipl

if this connects you you're good - to make sure things are sane try

	SELECT usename from pg_user;

If you want you can create your tables and such now

	CREATE TABLE todo (id INT , descxx VARCHAR(50));

 ctrl-c out

# Run the odbctest.pl

If you installed from ppa's on Debian you may need 

   sudo apt-get install swi-prolog-odbc 

	cd identiry/prolog/identity/store/sql/
	swipl odbctest.pl
	?- example.
	posts rows: row(0)
	posts rows: row(0)
	true.

If it instead gives you

	?- example.
	encoding name too long
	posts rows: row(0)
	posts rows: row(0)
	true.

First check that your odbc.ini is set to PostgreSQL Unicode
and not of PostgreSQL ANSI

If this doesn't fix, you need to set locales. 

# Set the locales

Brute force fix for encoding name too long, put this in your .bashrc
(but setting to correct of Unicode/ANSI above is better).

	export LANG="en_US.UTF-8"
	export LANGUAGE="en_US.UTF-8"
	export LC_CTYPE="en_US.UTF-8"
	export LC_NUMERIC="en_US.UTF-8"
	export LC_TIME="en_US.UTF-8"
	export LC_COLLATE="en_US.UTF-8"
	export LC_MONETARY="en_US.UTF-8"
	export LC_MESSAGES="en_US.UTF-8"
	export LC_PAPER="en_US.UTF-8"
	export LC_NAME="en_US.UTF-8"
	export LC_ADDRESS="en_US.UTF-8"
	export LC_TELEPHONE="en_US.UTF-8"
	export LC_MEASUREMENT="en_US.UTF-8"
	export LC_IDENTIFICATION="en_US.UTF-8"












