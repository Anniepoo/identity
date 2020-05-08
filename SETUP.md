
## Setup Guide

This is a cookbook guide to getting identity services running.

Our philosophy is that it's your project, we try to be as
minimally intrusive as possible. 

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
		      timeout(1800)  % session timeout as appropriate
		    ]),
		http_server(http_dispatch, [port(5000)]).
---

### Serve via TLS

This library is NOT secure when served via http! Set up https at least for all pages under /login

###  Setup static file handlers

You will need the normal js, css, and img handlers. You can either just
load library(identity/login_static) and let pack(identity) handle it, or
set them up yourself.

As long as the abstract paths `js(.)`, `css(.)`, and `img(.)` point
at the appropriate directories you'll be fine.

### Remove ResetDB

If you started by using the test code, remove the resetdb handler
or anyone on the web can truncate your database!
native
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

At minimum this set of compounds should be supported. 

 * password_hash(Hash) - A cryptographically secure pw hash
 * email(Email) - should be per-user unique
 * role(Role)
 * activation_key(Key) - one of poss. several valid keys to email activate the account

If at all possible storing arbitrary functors
should be supported, as the registration form roadmap includes adding arbitrary other info.
**Any functor may appear multiple times**. eg a user might have multiple roles.
This store is a convenient place to store other per_user data.

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

Default behavior is to only make a session when a user logs in.

When you start the http server do this before starting the server.

----
 http_set_session_options(
        [ create(noauto),
          timeout(1800)  % session length
        ]),
----

If you need
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
error page. See the error section of `library(identity/identity) for the default no-access page. If you would
like to create your own status page, copy this code and ensure it's loaded **after**
the identity pack is loaded.

### Styling

Set the setting `identity:style` to  style pages. The default is `default`.

Currently, the `.warning`, `.warn`, and `.error` classes are set with a style block at the end
of the registration page. This is likely to change soon.

### Changing Language

Most strings displayed to the user pass through `customize:local/2`.

By implementing the multifile `local_hook/2` you can alter most user messages. Return atoms.

### Custom Pages

You can make a higher priority version handler for any page.

In a manner similar to overriding the login form handler, you can override
login(register) to make a custom registration page. Additional fields in
the registration form are persisted into the user info database

### Log Out

You can make logout links by simply vemail not refilled when passwords don't match during regisisiting `/login/logout` while logged in.

By default this returns to the id `home`. If you want to go elsewhere, add a parameter `referer`.

### Display User Name

The inclusion `current_user//0` can be used in termerized html to display the user name.

### Activation Email

To require email account activation set setting identity:require_activation_email to true.

Because there are many possible ways to send email and one usually customizes the email body,
actually sending the email is left to the application developer.

Implement `login_email:activation_email_hook(UName, Email, Link)`, sending the email.

If not implemented the default sends the link to `debug/3`. This can be useful for debugging.

### Password Reset Email

You need to set some settings to set up 
:- setting(identity:reset_email_life, integer, 86400,
           "Time a password reset email is valid").

You may want to override any of these handlers with a higher priority handler.

% where the 'forgot my pw' link goes
:- http_handler(login(forgot), forgot_form_handler, [priority(-100), id(forgot)]).
% where user goes after entering email at form on above page
:- http_handler(login(doforgot), doforgot_form_handler, [priority(-100), id(doforgot)]).
% Where the emailed link takes the user
:- http_handler(login(resetpw), resetpw_form_handler, [prefix, priority(-100), id(resetpw)]).
% Where the form presented in resetpw takes the user
:- http_handler(login(doactualpwreset), do_actual_reset_handler, [priority(-100),
                                                               id(doactualpwreset)]).

You also need to install a hook to send the actual email.

:- multifile login_email:forgot_email_hook/3.

 login_email:forgot_email_hook(UName, Email, Link)

The default behavior only writes the information to `debug/3`

## Registration Validation

When registering, the user enters a username, email, and password. 

You need to set up validation (eg your site may not allow spaces in usernames, or may demand passwords be at least 8 characters long).

validation is controlled by a nested dict. This dict is a setting, `identity:constraints`.
See `login_validate.pl` for the default. Other than using messy regexes, it's fairly obvious.

## REST endpoints

REST endpoints only need the appropriate role in the http_handler options.

Pengines haven't been addressed yet.

