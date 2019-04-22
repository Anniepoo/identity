# Identity

An identity management system for SWI-Prolog.

# TL;DR

cd test/
swipl test.pl
?- go.
browse http://localhost:5000/

# Release 0.2.2

This release has all basic functionality working.

Testing has been minimal, but if you're adventurous, this is now the reasonable way to make a login and registration system in SWI-Prolog.

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

This software depends on SWI-Prolog 8.1.0 or later.

## Status

This library is definitely alpha software. It's currently close enough to production ready that
you could contribute a few days work and it would be ready for beta use.

## License

A license file should accompany this software. The intent is that
this software should be easily integrated with SWI-Prolog license.

Portions of this software incorporate code from the demo_login.pl example
distributed with `package-http`, part of **SWI-Prolog**.

## Setup Guide

The directory containing this file should also contain a SETUP.md file. This contains
a walk through of how to set the system up.

## Known Issues

 * BUG - click the secret link on the homepage. Login page comes up. After logging in, you don't go to the secret, you go home.
 * there are a few strings on server side field validation that aren't properly localized.

## TODO - many things are yet to do. This is our roadmap

### Access - Making it easier to use

 * add to README a list of everything you can configure - pages, strings, behavior
 * document all

### Code Quality

 * examine all TODOs in code
 * Move the error page at bottom of identity to its own module
 * look at doc_server for undocumented publics and bad format
 * add logging of various events
 * add debug/3 calls
 * make sure you're compatible with https://support.1password.com/compatible-website-design/


### Security and Reliability

 * Check out https://github.com/OWASP/CheatSheetSeries for instrusion safety
 * if you don't use some stuff in login_crypto for remember_me, remove it
 * IP throttle registration & login

### Testing

 * add real integration tests
 * Try overriding login, registration, and forbidden pages
 * test remember me against changing accounts
 * test mismatched passwords in registration
 * make rest endpoints work, or at least test that they do
 * make sure pengines work

###  Additional login options

 * add login via OAuth2  [simple guide to OAuth2 on web page](https://aaronparecki.com/oauth-2-simplified/#roles)
 * Allow 'username is email' - there are no true usernames, the username is just the users email.
 * admin logs in as user mode - to let admins change to being a user to help 'debug' user issues.
 * passwordless - to log in you give email and get a one-time link.

### Additional items on login/registration pages

 * Registration and/or login time CAPTCHA
 * 'remember me' - persistent cookie
   * make it work
   * expire remember me if user logs out
   * test remember me against changing accounts
 * validate uname char set to avoid faking via homoglyphs
 * password strength meter
 * Naughty word filtering for usernames
 * 'late login' - [Welie Web UX Patterns](http://www.welie.com/patterns/showPattern.php?patternID=login) defines late login. We already do this, but don't provide a reason. Add an option in the handler that defines an explanatory message why the user needs to log in. 
 * COPPA compliance
 * 2 Factor Authentication
   * SMS based
   * App based (eg with Authy, 1password, etc.)

### Additional Other Functionality

 * refuse to register duplicate emails (on an option) and user names.
 * Gravatar display
   * 'real' Gravatars
   * provide SWISH style svg-atars for those with sessions, logged in or not.
 * option on uname display inclusion that colors non-lower-ascii as a different color to prevent homoglyph spoofing
 * make a directive & another expansion that lets app programmer set a prefix as
requiring a role
 * Add a meta-inclusion that adds unauthorized class to link if user not authorized
 * Add an ajax endpoint that says yes/no an endpoint is authorized (pengine? naw, ajax)
 * native app support


### Expert system

It would be useful to have an expert system or _wizard_ that both leads the user through setup and 
performs a security check.

We really just need a list of things that must be true, a list of goals, some that share variables,
and we start at the front and call each. If it fails, we call make_true(Goal), and that interacts with the user
to make the thing true.

But the list of goals could change (eg if you allow pw reset you must setup the backend email server),
so maybe it's more like an adventure game, where you must walk through the map, and it presents choices
and has planner like actions. Then automatically take the action when only one is possible.

Alternatively, we could have 2 phases - first we ask the user what they want to do, and then we 
copile what they need to do, and guide them through doing it. And maybe they can save their work
in the second part. 


### Separate pack?

It might be best to make a separate pack that does 

 * profile management - keeping track of a bunch of partially public data and allowing the user to update
 * gravatars - move the gravatar/svg-atar functionality to here
 * allows profile image upload
 * display names - separate from user names
 * gamification rating system like Stack Overflow
 * and other user management not strictly related to identity.
 * GDPR compliance / first ttime popup



# other web design patterns

Far down the road, pack(identity) could become one of a family of packs that work together

 * more general identity - profiles, display name, gravatar, gamification rating
 * auto generated CRUD similar to Rails scaffold
 * 'flash' pattern from rails, where the previous page's 'flash' is available
 * Hasura like 'watch this thing' against a knowledgebase
 * Site wide maintainence mode - ability to turn on 'sorry, we're down'
 * blog - similar to Raivo's blog core (maybe just integrate blog-core)
 * CMS
 * store
 * map roi
 * graph creation tool.
 * threading, up/down voting, moderators, tagging, rate-limits, quote-replying, retweeting, crossposting.
 * naughty word filtering - more general than just usernames
 * sentiment analysis to keep hate off site
 * anti-spam/anti-griefing assistance
 * customizable window - moodle like draggable content


# DONE

* allow changing password
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
* Add  checks on uname/pw size
* password forgot is stubbed in, Add pw reset email
 * Add assistance to prevent CSRF - app programmer adds an inclusion in the form as a hidden field, and the matching id handler checks if it's a matching hash.   https://github.com/OWASP/CheatSheetSeries/blob/master/cheatsheets/Cross-Site_Request_Forgery_Prevention_Cheat_Sheet.md
Can this be enforced by static analysis?
 * move all strings in default pages to some strings override predicate












