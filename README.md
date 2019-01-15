# Identity

An identity management system for SWI-Prolog.

# What it is

Almost every web application has the notion of a user, with an account,
a profile, and some privileges.

And signing up for and maintaining this is pretty much done the same
way, with slight variations, everywhere.

And it can get messy and complex to do. There's no reason for everybody
to do it over and over.

I need a library like this. I'm writing one.

Almost everything in this library **might** need customizing. I will try to be
good about promptly accepting PRs for needed hooks. Please contact the lead
maintainer (Anniepoo on github, anne@swi-prolog.org by email) before implementing
such so we can maintain some uniformity.

And forking and hacking on this or just stealing parts are valid.

## Version

This software depends on SWI-Prolog 7.7.26 or later. As of this writing, 
that release is not yet released, so you will need to run from HEAD.

## License

A license file should accompany this software. The intent is that
this software should be easily integrated with SWI-Prolog license.

Portions of this software incorporate code from the demo_login.pl example
distributed with `package-http`, part of **SWI-Prolog**.

## Bluffers Guide

This is a cookbook guide to getting identity services running.

You will need to ensure the identity library is loaded.

---
    :- use_module(library(identity/identity)).
---

Set your session options to noauto prior to starting the server.

---
    http_set_session_options(
        [ create(noauto)
        ]),
    http_server(http_dispatch, [port(5000)]).
---

Add **role** option to any handler that requires login. This can be any
roles you want. A common set would be [user, admin]. Non logged in users
can access only pages without a role.

Default behavior is to only make a session when a user logs in. If you need
sessions for guests I suggest you figure out what that should look like and
send us a PR.

---
   :- http_handler(root(secret), secret_handler, [id(secret), role(user)]).
---

By default, all login happens on URI path `/login`. To move it, assign a higher
priority user:location in another place

---
    :- multifile http:location/3.
    http:location(login, root(mylogin), [priority(0)]).
---

If your user doesn't have access to the page, they will be directed to an
error page. See the error section for the default no-access page. If you would
like to create your own status page, copy this code and ensure it's loaded **after**
the identity pack is loaded.

TODO - set the length of time the user remains logged in via 'remember me'

If you want a custom login page, define a new handler like

---
    :- http_handler(login(.), my_login_form_handler,
               [id(login_form), identity(guest), priority(10)]).
---

and modify a copy of `login_page:login_form_handler` to make `my_login_form_handler`.

You can make logout links by simply visiting `/login/logout` while logged in.

By default this returns to the id `home`. If you want to go elsewhere, add a parameter `referer`.

In a manner similar to overriding the login form handler, you can override 
login(register) to make a custom registration page. Additional fields in
the registration form are persisted into the user info.  

TODO - explain how this works

TODO - explain how to wire up the database.


## Overall

## Roles

### guest

### logged_in

### not_activated

### admin

### nobody
(used to blank out sections)


## REST endpoints


## Use with 'stock' authentication

## TODO

 * add to README
 * clean up all the redundancy in `login_database`, use `user_property`
 * make real persist API 
 * make default persist API
 * add real integration tests
 * add email activation
 * add styling support
 * add OAuth
 * Add  checks on uname/pw size
 * Add pw reset email
 * document all
 * Add a meta-inclusion that adds unauthorized class to link if user not authorized
 * Add a pengine endpoint that says yes/no an endpoint is authorized
 * Try overriding login, registration, and forbidden pages
 * Move the error page at bottom of identity to its own module
 * make rest endpoints work, or at least test that they do
 * make sure pengines work
 * I think login_crypto is dead, remove
 * handle username is email
 * handle registration invalid (eg dup email)
 * make remember me cookie work
 * expire remember me if user logs out
 * test remember me against changing accounts
 * handle mismatched passwords in registration
 * Gravatars
 * CAPTCHA
 * validate uname char set to avoid faking (display non ascii in color?)
 * guests w/ sessions can get swish svg-atars
 * maybe sep. pack with svg-atars and settings page
 * password strength meter
 * look at doc_server for undoced publics and bad format
 * move all strings in default pages to some strings override predicate
 * prevent adding multiple users
 * add logging of various events
 * add debug/3 calls



# other web design patterns

 * more general identity - profiles, gravatar, zwinkies, gamification rating
 * Rails scaffold 
 * flash pattern from rails
 * Hasura like 'watch this thing' against a knowledgebase
 * Site wide maintainence
 * blog
 * CMS
 * store
 * map roi
 * threading, up/down voting, moderators, tagging, rate-limits, quote-replying, retweeting, crossposting. 
 





 


 



