# Log of the mail list changes re http_dispatch

i,

Following the extensions of http_dispatch, I have cleaned up the server
infrastructure to make it more accessible.  These are the steps:

  - Remove xpce and inetd connection managers, leaving only
    thread_httpd.pl.  My assumption is that the others are not
    used or can easily be replaced.  If you have a good use case
    for the old managers, please report.

  - Added a new library(http/http_server) that simply loads and
    reexports what I consider the core server libraries, so you
    no longer have to remember some of their awkward names and
    can be up and running with a few lines of code.  We can
    consider adding some more.  The current set is:

    - thread_httpd.pl
    - http_wrapper.pl
    - http_dispatch.pl
    - http_dyn_workers.pl (see below)
    - html_write.pl
    - http_json.pl

  - Added http_dyn_workers.pl, copied from SWISH.  This plugin provides
    dynamic management of the worker pool, using few resources for hardly
    used servers while scaling for heavy workloads without requiring the
    user to get the configuration right.

I think this should bring the core infrastrucure in good shape for
launching SWI-Prolog 8.0.  If you see omisions that can easily be
fixed without disturbing stability and conpatibility, please step
forwards.

    Cheers --- Jan

==================

Hi Anne, Raivo and other HTTP server users,

Pushed a first version.  You find examples handling login and routing
for REST APIs at
https://github.com/SWI-Prolog/packages-http/tree/master/examples, files
demo_login.pl and demo_rest.pl

If you rebuild using CMake, help/1 should give the updated
documentation, notably use the following.  The first describes the steps
taken by http_dispatch/1.

    ?- help(http_dispatch).
    ?- help(http_handler).
    ?- help(http_request_expansion).

Some of this requires a tutorial rather than the reference manual.  For
now, the demos should suffice.

Please comment, so things can be fixed before the use is widespread.

    Cheers --- Jan

==============


On 01/01/2019 16:31, Jan Wielemaker wrote:
> Hi Anne,
>
> I've started some hacking.  First added Raivo's arouter construct
> that allows for e.g.,
>
> :- http_handler(root(user/User), user(M, User),
>          [ method(M),
>            methods([get,post,delete])
>          ]).
>
> user(Method, User, _Request) :-
>      format('Content-type: text/plain~n~n'),
>      format('Run method ~p on user ~p~n', [Method, User]).
>
> Which works just great. Thanks for that idea Raivo. I was considering
> allowing for the registration of multiple handlers on the same path with
> different methods, but more or less decided against this. It makes the
> rules on which handler fires, finding implementations using
>
>      ?- edit('/user/').
>
> more complicated and adds nothing worthwhile.
>
> Next will be the request rewriting pipeline. That doesn't seem very
> hard, although some of your use cases may need some iterations to get
> right.
>
> Success moving
>
>      Cheers --- Jan
>
>
> On 31/12/2018 19:57, Anne Ogborn wrote:
>>> There are not a `few' calls to http_server/2. Typically there is one,
>>
>>
>> unless the system listens on multiple ports (e.g., http and https).
>>
>> That's what I meant by "a few". If you're running a web application,
>> ciiopatria, and the doc_server
>> it might have been called 3 times on 3 ports.
>>
>>
>>  >That is also my point: starting the server normally happens once
>> somewhere, often outside the control of the application itself (e.g.,
>> using http_unix_daemon.pl).  That one call may need to support multiple
>> web applications using their own requirements wrt. extensions.  Hooks
>> are the common template to deal with this style of modularity.
>>
>>
>> Ah! That's a good argument for not doing it like I am now.
>>
>>
>>  > Some use cases:
>>  >
>>  >      * authentication
>>
>> Can work. Current HTTP authentication uses exactly what I propose, only
>> hardwired as the one and only member of what could be a pipeline.  Oauth
>> or login page must be supported.
>>
>> Yes - really, nobody has a gripe as much with the architecture as with
>> there being
>> inadequate ways to insert exception handlers.
>>
>>  >      * system status - a wrapper that dispatches all html calls to
>> some status page,
>>  > and dispatches REST calls to some refusal handler.
>>
>> You mean for maintenance or something like that? The current system
>> already allows for that by dynamically registering a prefix handler at /
>> that hides its children and has a high priority. When done you just
>> remove this handler.
>>
>> Yes, suppose the site goes down - popping up a 'we're down' message
>> prevents
>> people from doing retries, and looks more professional.
>>
>>  >      * cache busting
>>
>> We can add a feature that allows for extending the reply header.  An
>> addition to the current CGI style is probably desirable anyway.
>>
>> That's needed for MANY things. The login page is directly writing the
>> 302 response
>> Another issue - changing the actual response code, not the header, is
>> quite painful.
>> Using any of the nice stuff in the framework is contingent on returning
>> a 200 code.
>>
>> I'm moving, so it may be a couple of days before I'm back involved in
>> this.
>>
>> Annie
>>
>>
>>
>>
>



