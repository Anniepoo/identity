# Identity

An identity management system for SWI-Prolog.

# What it is

Almost every web application has the notion of a user, with an account,
a profile, and some privileges.

And signing up for and maintaining this is pretty much done the same
way, with slight variations, everywhere.

And it can get messy and complex to do. There's no reason for everybody
to do it over and over.

I needed a library like this, so I wrote one.

Almost everything in this library **might** need customizing. I will try to be
good about promptly accepting PRs for needed hooks. Please contact the lead
maintainer (Anniepoo on github, anne@swi-prolog.org by email) before implementing
such so we can maintain some uniformity.

And forking and hacking on this or just stealing parts are valid.

## Version

This software depends on SWI-Prolog 7.7.26 or later. As of this writing, 
that release is not yet released, so you will need to run from HEAD.

## Status

This library is definitely alpha software. It's currently close enough to production ready that
you could contribute a few days work and it would be ready for beta use.

## License

A license file should accompany this software. The intent is that
this software should be easily integrated with SWI-Prolog license.

Portions of this software incorporate code from the demo_login.pl example
distributed with `package-http`, part of **SWI-Prolog**.

## Bluffer's Guide

This is a cookbook guide to getting identity services running.

### General Setup

You will need to ensure the identity library is loaded.

---
    pack_install(identity).
---

If you're reading this document you probably already did this.

Now you can load identity.

---
    :- use_module(library(identity/identity)).
---

We won't need sessions, generally, for users not logged in.
Set your session options to `create(noauto)` prior to starting the server. 
You might also want to increase the session time.

---
go :-
    use_default_db,
    http_set_session_options(
        [ create(noauto),
          timeout(1800)  % half hour sessions
        ]),
    http_server(http_dispatch, [port(5000)]).
---

### Attach Database

Identity is agnostic about how you store your user data.

A simple choice is the default back end. Call `use_default_db` before starting the server.
User data will be persisted to the file users.db using `library(persistancy)`.
This solution is adequate for 100,000 users or so.

If you need something else, `library(identity/login_database)` provides this set of multifile predicates.

---
:- multifile
    user_property_expansion/2,
    set_user_property_expansion/2,
    assert_user_property_expansion/2,
    retract_user_property_expansion/2,
    retractall_user_property_expansion/2.
---

Each takes the arguments `UserName` and a compound. They make a key-value store in the obvious way.

All second arg values are compounds. Currently all are of arity 1. 

At minimum this set of compounds should be supported. If at all possible storing arbitrary functors
should be supported, as the registration form roadmap includes adding arbitrary other info.
Any functor may appear multiple times. eg a user might have multiple roles.
This store is a convenient place to store other per_user data.

 * password_hash(Hash) - A cryptographically secure pw hash
 * email(Email) - should be per-user unique
 * role(Role)
 * activation_key(Key) - one of poss. several valid keys to email activate the account


These user roles are known. 

 * needs_activation - if this role is present the user is redirected to the needs activation page
 * user -logged in users who can do normal things

When a user has logged in, completed 2FA, activated their account, isn't banned, and so on, they 
have role(user). They may have additional roles (admin, teacher, ...).

### Set Access

Add a **role/1** option to any handler that requires login. This can be any
roles you want. A common set would be [user, admin], and most 'normal' pages would be `role(user)`. 
Non logged in users can access only pages without a role.

### Sessions

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

### Styling

Set the setting `identity:style` to  style pages. The default is `default`.

### Changing Language

Most strings displayed to the user pass through `customize:local/2`.

By implementing the multifile local_hook/2 you can alter most user messages. Return atoms.

### Custom Pages

You can make a higher priority version handler for any page. 

In a manner similar to overriding the login form handler, you can override 
login(register) to make a custom registration page. Additional fields in
the registration form are persisted into the user info database

### Log Out

You can make logout links by simply visiting `/login/logout` while logged in.

By default this returns to the id `home`. If you want to go elsewhere, add a parameter `referer`.

### Display User Name

The inclusion `current_user//0` can be used in termerized html to display the user name.

### Activation Email

To require email account activation set setting identity:require_activation_email to true.

Because there are many possible ways to send email and one usually customizes the email body,
actually sending the email is left to the application developer.

Implement `login_email:activation_email_hook(UName, Email, Link)`, sending the email.

If not implemented the default sends the link to `debug/3`. This can be useful for debugging.

## REST endpoints


## TODO - many things are yet to do

 * add to README
      * list of everything you can configure - pages, strings, behavior
  * BUG - click the secret link on the homepage. Login page comes up. After logging in, you don't go to the secret, you go home.
 * add real integration tests
 * add OAuth
 * Add  checks on uname/pw size
 * Add pw reset email
 * document all
 * make a directive & another expansion that lets app programmer set a prefix as
requiring a role
 * Add a meta-inclusion that adds unauthorized class to link if user not authorized
 * Add a pengine endpoint that says yes/no an endpoint is authorized
 * Try overriding login, registration, and forbidden pages
 * Move the error page at bottom of identity to its own module
 * make rest endpoints work, or at least test that they do
 * make sure pengines work
 * if you don't use some stuff in login_crypto for remember_me, remove it
 * handle username is email
 * handle registration invalid (eg dup email)
 * make remember me cookie work
 * expire remember me if user logs out
 * test remember me against changing accounts
 * handle mismatched passwords in registration
 * allow changing password
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
 * IP throttle registration & login

# DONE

 * clean up all the redundancy in `login_database`, use `user_property`
 * make real persist API 
 * make default persist API
* Design how configuration works. Don't just let it happen.
      * global settings - use settings library
      * how initialization works  (lazy?, application programmer calls?) - choose ap programmer
      * strings - local//1 works
      * page layout - override handler
 * add email activation
 * add debug email - just prints debug message
 * add styling support



# other web design patterns

 * more general identity - profiles, display name, gravatar, zwinkies, gamification rating
 * Rails scaffold 
 * flash pattern from rails
 * Hasura like 'watch this thing' against a knowledgebase
 * Site wide maintainence
 * blog
 * CMS
 * store
 * map roi
 * threading, up/down voting, moderators, tagging, rate-limits, quote-replying, retweeting, crossposting. 
 





 


 



