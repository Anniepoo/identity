:- module(login_remember, [

          ]).
/** <module> Remember Me check functionality
 *
 * Not working yet
 *
 * Design:
 *
 * if remember_me is checked, do_actual_login provides an additional
 * cookie. I think I asked on Google version swipl list how
 * to do this.
 *
 * Provide an additional http_request_expansion in identity.pl
 * with priority 150. If the user  provides a remember_me cookie
 * do a redirect to a special version of login_page. This
 * will look at the remember-me and then call do_actual_login
 * with an ok/invalid status code and a referer of the original page
 *
 * logout link will kill the remember_me cookie.
 * NTO NEEDED: So will logging in with uname_pw without the remember_me
  * checked
  *
  * The remember_me checkbox html//1 inclusion moves here??
  *
  */


 % TODO - make sure logout kills the remember me cookie
