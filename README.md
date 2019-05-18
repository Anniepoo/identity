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

## Development

To do meaningful development you need to be running https and be visible to the outside world.

[Writeup how to make your local server visible on the web](https://cutebouncingbunnies.wordpress.com/2014/01/02/how-to-run-a-server-on-your-desktop-using-ssh/)

(If you need a server to do dev on this talk to Annie)

You will then have to terminate TLS on the public server. TODO

Forward your machine to the public machine

ssh -R 0.0.0.0:8866:localhost:5000 -N anniepoo@partyserver.rocks

Do something I haven't figured out to get http -> http there

## Known Issues

 * BUG - click the secret link on the homepage. Login page comes up. After logging in, you don't go to the secret, you go home.
 * there are a few strings on server side field validation that aren't properly localized.












