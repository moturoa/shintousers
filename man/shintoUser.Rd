% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/users_r6_object.R
\name{shintoUser}
\alias{shintoUser}
\title{A User object for use in Shinto apps}
\value{
Returns (invisibly) the last login information
}
\description{
Make a database connection to 'shintousers' and access all kinds of
methods for user management. In Shiny apps, use `get_and_set_last_login`, for example.
}
\section{Super class}{
\code{shintodb::NA} -> \code{ShintoUsers}
}
\section{Public fields}{
\if{html}{\out{<div class="r6-fields">}}
\describe{
\item{\code{con}}{The DB connection to `shintousers`}

\item{\code{schema}}{The postgres schema in the DB}

\item{\code{dbusername}}{The DB user name}

\item{\code{userid}}{The application (shiny) user name}

\item{\code{appname}}{The (rsconnect) application name}

\item{\code{appversion}}{Version of the app (optionally read from VERSION)}
}
\if{html}{\out{</div>}}
}
\section{Methods}{
\subsection{Public methods}{
\itemize{
\item \href{#method-ShintoUsers-new}{\code{shintoUser$new()}}
\item \href{#method-ShintoUsers-get_shiny_user}{\code{shintoUser$get_shiny_user()}}
\item \href{#method-ShintoUsers-to_json}{\code{shintoUser$to_json()}}
\item \href{#method-ShintoUsers-from_json}{\code{shintoUser$from_json()}}
\item \href{#method-ShintoUsers-get_name}{\code{shintoUser$get_name()}}
\item \href{#method-ShintoUsers-get_last_login}{\code{shintoUser$get_last_login()}}
\item \href{#method-ShintoUsers-set_last_login}{\code{shintoUser$set_last_login()}}
\item \href{#method-ShintoUsers-get_and_set_last_login}{\code{shintoUser$get_and_set_last_login()}}
\item \href{#method-ShintoUsers-app_has_user}{\code{shintoUser$app_has_user()}}
\item \href{#method-ShintoUsers-list_application_users}{\code{shintoUser$list_application_users()}}
\item \href{#method-ShintoUsers-set_user_attributes}{\code{shintoUser$set_user_attributes()}}
\item \href{#method-ShintoUsers-get_user_attributes}{\code{shintoUser$get_user_attributes()}}
\item \href{#method-ShintoUsers-get_role}{\code{shintoUser$get_role()}}
\item \href{#method-ShintoUsers-set_role}{\code{shintoUser$set_role()}}
\item \href{#method-ShintoUsers-set_user_active_inactive}{\code{shintoUser$set_user_active_inactive()}}
\item \href{#method-ShintoUsers-disable_user}{\code{shintoUser$disable_user()}}
\item \href{#method-ShintoUsers-enable_user}{\code{shintoUser$enable_user()}}
\item \href{#method-ShintoUsers-get_group}{\code{shintoUser$get_group()}}
\item \href{#method-ShintoUsers-is_in_group}{\code{shintoUser$is_in_group()}}
\item \href{#method-ShintoUsers-set_group}{\code{shintoUser$set_group()}}
\item \href{#method-ShintoUsers-remove_role}{\code{shintoUser$remove_role()}}
\item \href{#method-ShintoUsers-has_role}{\code{shintoUser$has_role()}}
\item \href{#method-ShintoUsers-is_admin}{\code{shintoUser$is_admin()}}
\item \href{#method-ShintoUsers-add_application}{\code{shintoUser$add_application()}}
\item \href{#method-ShintoUsers-set_application_roles}{\code{shintoUser$set_application_roles()}}
\item \href{#method-ShintoUsers-set_application_groups}{\code{shintoUser$set_application_groups()}}
\item \href{#method-ShintoUsers-get_application_roles}{\code{shintoUser$get_application_roles()}}
\item \href{#method-ShintoUsers-get_application_groups}{\code{shintoUser$get_application_groups()}}
\item \href{#method-ShintoUsers-get_applications}{\code{shintoUser$get_applications()}}
\item \href{#method-ShintoUsers-remove_application}{\code{shintoUser$remove_application()}}
\item \href{#method-ShintoUsers-log_timing}{\code{shintoUser$log_timing()}}
\item \href{#method-ShintoUsers-clone}{\code{shintoUser$clone()}}
}
}
\if{html}{\out{
<details><summary>Inherited methods</summary>
<ul>
<li><span class="pkg-link" data-pkg="shintodb" data-topic="NA" data-id="append_data"><a href='../../shintodb/html/NA.html#method-NA-append_data'><code>shintodb::NA$append_data()</code></a></span></li>
<li><span class="pkg-link" data-pkg="shintodb" data-topic="NA" data-id="close"><a href='../../shintodb/html/NA.html#method-NA-close'><code>shintodb::NA$close()</code></a></span></li>
<li><span class="pkg-link" data-pkg="shintodb" data-topic="NA" data-id="connect_to_database"><a href='../../shintodb/html/NA.html#method-NA-connect_to_database'><code>shintodb::NA$connect_to_database()</code></a></span></li>
<li><span class="pkg-link" data-pkg="shintodb" data-topic="NA" data-id="delete_rows_where"><a href='../../shintodb/html/NA.html#method-NA-delete_rows_where'><code>shintodb::NA$delete_rows_where()</code></a></span></li>
<li><span class="pkg-link" data-pkg="shintodb" data-topic="NA" data-id="execute_query"><a href='../../shintodb/html/NA.html#method-NA-execute_query'><code>shintodb::NA$execute_query()</code></a></span></li>
<li><span class="pkg-link" data-pkg="shintodb" data-topic="NA" data-id="filter"><a href='../../shintodb/html/NA.html#method-NA-filter'><code>shintodb::NA$filter()</code></a></span></li>
<li><span class="pkg-link" data-pkg="shintodb" data-topic="NA" data-id="get_query"><a href='../../shintodb/html/NA.html#method-NA-get_query'><code>shintodb::NA$get_query()</code></a></span></li>
<li><span class="pkg-link" data-pkg="shintodb" data-topic="NA" data-id="has_connection"><a href='../../shintodb/html/NA.html#method-NA-has_connection'><code>shintodb::NA$has_connection()</code></a></span></li>
<li><span class="pkg-link" data-pkg="shintodb" data-topic="NA" data-id="has_value"><a href='../../shintodb/html/NA.html#method-NA-has_value'><code>shintodb::NA$has_value()</code></a></span></li>
<li><span class="pkg-link" data-pkg="shintodb" data-topic="NA" data-id="have_column"><a href='../../shintodb/html/NA.html#method-NA-have_column'><code>shintodb::NA$have_column()</code></a></span></li>
<li><span class="pkg-link" data-pkg="shintodb" data-topic="NA" data-id="list_indexes"><a href='../../shintodb/html/NA.html#method-NA-list_indexes'><code>shintodb::NA$list_indexes()</code></a></span></li>
<li><span class="pkg-link" data-pkg="shintodb" data-topic="NA" data-id="list_tables"><a href='../../shintodb/html/NA.html#method-NA-list_tables'><code>shintodb::NA$list_tables()</code></a></span></li>
<li><span class="pkg-link" data-pkg="shintodb" data-topic="NA" data-id="make_column"><a href='../../shintodb/html/NA.html#method-NA-make_column'><code>shintodb::NA$make_column()</code></a></span></li>
<li><span class="pkg-link" data-pkg="shintodb" data-topic="NA" data-id="postgres_now"><a href='../../shintodb/html/NA.html#method-NA-postgres_now'><code>shintodb::NA$postgres_now()</code></a></span></li>
<li><span class="pkg-link" data-pkg="shintodb" data-topic="NA" data-id="postgres_now_in_local_time"><a href='../../shintodb/html/NA.html#method-NA-postgres_now_in_local_time'><code>shintodb::NA$postgres_now_in_local_time()</code></a></span></li>
<li><span class="pkg-link" data-pkg="shintodb" data-topic="NA" data-id="query"><a href='../../shintodb/html/NA.html#method-NA-query'><code>shintodb::NA$query()</code></a></span></li>
<li><span class="pkg-link" data-pkg="shintodb" data-topic="NA" data-id="read_table"><a href='../../shintodb/html/NA.html#method-NA-read_table'><code>shintodb::NA$read_table()</code></a></span></li>
<li><span class="pkg-link" data-pkg="shintodb" data-topic="NA" data-id="replace_value_where"><a href='../../shintodb/html/NA.html#method-NA-replace_value_where'><code>shintodb::NA$replace_value_where()</code></a></span></li>
<li><span class="pkg-link" data-pkg="shintodb" data-topic="NA" data-id="replace_value_where2"><a href='../../shintodb/html/NA.html#method-NA-replace_value_where2'><code>shintodb::NA$replace_value_where2()</code></a></span></li>
<li><span class="pkg-link" data-pkg="shintodb" data-topic="NA" data-id="table_columns"><a href='../../shintodb/html/NA.html#method-NA-table_columns'><code>shintodb::NA$table_columns()</code></a></span></li>
<li><span class="pkg-link" data-pkg="shintodb" data-topic="NA" data-id="table_info"><a href='../../shintodb/html/NA.html#method-NA-table_info'><code>shintodb::NA$table_info()</code></a></span></li>
<li><span class="pkg-link" data-pkg="shintodb" data-topic="NA" data-id="update_where"><a href='../../shintodb/html/NA.html#method-NA-update_where'><code>shintodb::NA$update_where()</code></a></span></li>
<li><span class="pkg-link" data-pkg="shintodb" data-topic="NA" data-id="write_table_overwrite"><a href='../../shintodb/html/NA.html#method-NA-write_table_overwrite'><code>shintodb::NA$write_table_overwrite()</code></a></span></li>
</ul>
</details>
}}
\if{html}{\out{<hr>}}
\if{html}{\out{<a id="method-ShintoUsers-new"></a>}}
\if{latex}{\out{\hypertarget{method-ShintoUsers-new}{}}}
\subsection{Method \code{new()}}{
Make new shintousers object
\subsection{Usage}{
\if{html}{\out{<div class="r">}}\preformatted{shintoUser$new(
  userid = NULL,
  appname = "",
  appversion = "",
  default_user = "unknown",
  ad_groups = NULL,
  ad_authentication = FALSE,
  admin_group_pattern = "",
  con = NULL,
  pool = FALSE,
  dbusername = NULL,
  dbname = NULL,
  ...
)}\if{html}{\out{</div>}}
}

\subsection{Arguments}{
\if{html}{\out{<div class="arguments">}}
\describe{
\item{\code{userid}}{rsconnect username, if not NULL it is stored and used for all methods (handy inside an app)}

\item{\code{appname}}{rsconnect application name}

\item{\code{appversion}}{Optional, application version string}

\item{\code{default_user}}{Optional, go-to username if nothing is filled in}

\item{\code{ad_groups}}{Optional, active directory-groups}

\item{\code{ad_authentication}}{Boolean that checks if active directory authentication has to be done}

\item{\code{admin_group_pattern}}{Optional pattern to see which groupnames are implicating admin users}

\item{\code{con}}{Optional, existing database connection to shintousers (for recycling)}

\item{\code{pool}}{Passed to [shintodb::connect()]}

\item{\code{dbusername}}{ignored ("shintousers")}

\item{\code{dbname}}{ignored ("shintousers")}

\item{\code{...}}{Further arguments passed to [shintodb::connect()]}
}
\if{html}{\out{</div>}}
}
\subsection{Returns}{
A 'shintousers' R6 object
}
}
\if{html}{\out{<hr>}}
\if{html}{\out{<a id="method-ShintoUsers-get_shiny_user"></a>}}
\if{latex}{\out{\hypertarget{method-ShintoUsers-get_shiny_user}{}}}
\subsection{Method \code{get_shiny_user()}}{
Get current shiny user
\subsection{Usage}{
\if{html}{\out{<div class="r">}}\preformatted{shintoUser$get_shiny_user(default, session = shiny::getDefaultReactiveDomain())}\if{html}{\out{</div>}}
}

\subsection{Arguments}{
\if{html}{\out{<div class="arguments">}}
\describe{
\item{\code{default}}{Default user (returned if user is NULL)}

\item{\code{session}}{Shiny session object (no need to set)}
}
\if{html}{\out{</div>}}
}
}
\if{html}{\out{<hr>}}
\if{html}{\out{<a id="method-ShintoUsers-to_json"></a>}}
\if{latex}{\out{\hypertarget{method-ShintoUsers-to_json}{}}}
\subsection{Method \code{to_json()}}{
Convert to JSON
\subsection{Usage}{
\if{html}{\out{<div class="r">}}\preformatted{shintoUser$to_json(x)}\if{html}{\out{</div>}}
}

\subsection{Arguments}{
\if{html}{\out{<div class="arguments">}}
\describe{
\item{\code{x}}{An object to convert to JSON}
}
\if{html}{\out{</div>}}
}
}
\if{html}{\out{<hr>}}
\if{html}{\out{<a id="method-ShintoUsers-from_json"></a>}}
\if{latex}{\out{\hypertarget{method-ShintoUsers-from_json}{}}}
\subsection{Method \code{from_json()}}{
Convert to JSON
\subsection{Usage}{
\if{html}{\out{<div class="r">}}\preformatted{shintoUser$from_json(txt)}\if{html}{\out{</div>}}
}

\subsection{Arguments}{
\if{html}{\out{<div class="arguments">}}
\describe{
\item{\code{txt}}{Text string, JSON}
}
\if{html}{\out{</div>}}
}
}
\if{html}{\out{<hr>}}
\if{html}{\out{<a id="method-ShintoUsers-get_name"></a>}}
\if{latex}{\out{\hypertarget{method-ShintoUsers-get_name}{}}}
\subsection{Method \code{get_name()}}{
Read attribute 'naam' for a user (default = current user)
\subsection{Usage}{
\if{html}{\out{<div class="r">}}\preformatted{shintoUser$get_name(userid = NULL, appname = NULL)}\if{html}{\out{</div>}}
}

\subsection{Arguments}{
\if{html}{\out{<div class="arguments">}}
\describe{
\item{\code{userid}}{Vector of user ID's}

\item{\code{appname}}{Application name (can be NULL, appname on init is then used)}
}
\if{html}{\out{</div>}}
}
}
\if{html}{\out{<hr>}}
\if{html}{\out{<a id="method-ShintoUsers-get_last_login"></a>}}
\if{latex}{\out{\hypertarget{method-ShintoUsers-get_last_login}{}}}
\subsection{Method \code{get_last_login()}}{
Get last login for this user for this application (reads `shintousers.logins`)
\subsection{Usage}{
\if{html}{\out{<div class="r">}}\preformatted{shintoUser$get_last_login(userid = NULL, appname = NULL)}\if{html}{\out{</div>}}
}

\subsection{Arguments}{
\if{html}{\out{<div class="arguments">}}
\describe{
\item{\code{userid}}{rsconnect username}

\item{\code{appname}}{rsconnect application name}
}
\if{html}{\out{</div>}}
}
}
\if{html}{\out{<hr>}}
\if{html}{\out{<a id="method-ShintoUsers-set_last_login"></a>}}
\if{latex}{\out{\hypertarget{method-ShintoUsers-set_last_login}{}}}
\subsection{Method \code{set_last_login()}}{
Update the last login for this user / appname (in table `shintousers.logins`)
\subsection{Usage}{
\if{html}{\out{<div class="r">}}\preformatted{shintoUser$set_last_login(
  now = as.character(Sys.time()),
  userid = NULL,
  appname = NULL,
  appversion = NULL
)}\if{html}{\out{</div>}}
}

\subsection{Arguments}{
\if{html}{\out{<div class="arguments">}}
\describe{
\item{\code{now}}{Optional, time string (defaults to sys time)}

\item{\code{userid}}{rsconnect username (can be NULL, uses userid on init)}

\item{\code{appname}}{rsconnect application name}

\item{\code{appversion}}{application version string}
}
\if{html}{\out{</div>}}
}
}
\if{html}{\out{<hr>}}
\if{html}{\out{<a id="method-ShintoUsers-get_and_set_last_login"></a>}}
\if{latex}{\out{\hypertarget{method-ShintoUsers-get_and_set_last_login}{}}}
\subsection{Method \code{get_and_set_last_login()}}{
Reads last login, and sets the current time as the new 'last login'.
\subsection{Usage}{
\if{html}{\out{<div class="r">}}\preformatted{shintoUser$get_and_set_last_login(
  userid = NULL,
  appname = NULL,
  appversion = NULL
)}\if{html}{\out{</div>}}
}

\subsection{Arguments}{
\if{html}{\out{<div class="arguments">}}
\describe{
\item{\code{userid}}{rsconnect username (can be NULL, uses userid on init)}

\item{\code{appname}}{rsconnect application name}

\item{\code{appversion}}{application version string}
}
\if{html}{\out{</div>}}
}
\subsection{Details}{
If user has never logged in, writes a new line in `shintousers.logins`,
otherwise updates the last login.
}

}
\if{html}{\out{<hr>}}
\if{html}{\out{<a id="method-ShintoUsers-app_has_user"></a>}}
\if{latex}{\out{\hypertarget{method-ShintoUsers-app_has_user}{}}}
\subsection{Method \code{app_has_user()}}{
Does an app have a user configured?
\subsection{Usage}{
\if{html}{\out{<div class="r">}}\preformatted{shintoUser$app_has_user(userid, appname)}\if{html}{\out{</div>}}
}

\subsection{Arguments}{
\if{html}{\out{<div class="arguments">}}
\describe{
\item{\code{userid}}{rsconnect username (can be NULL, uses userid on init)}

\item{\code{appname}}{rsconnect application name}
}
\if{html}{\out{</div>}}
}
}
\if{html}{\out{<hr>}}
\if{html}{\out{<a id="method-ShintoUsers-list_application_users"></a>}}
\if{latex}{\out{\hypertarget{method-ShintoUsers-list_application_users}{}}}
\subsection{Method \code{list_application_users()}}{
List users for an app
\subsection{Usage}{
\if{html}{\out{<div class="r">}}\preformatted{shintoUser$list_application_users(
  appname = NULL,
  roles = NULL,
  groups = NULL,
  active_only = TRUE,
  ignore_groups = NULL,
  by_group = FALSE,
  add_last_login = FALSE
)}\if{html}{\out{</div>}}
}

\subsection{Arguments}{
\if{html}{\out{<div class="arguments">}}
\describe{
\item{\code{appname}}{rsconnect application name}

\item{\code{roles}}{rolnames to filter on}

\item{\code{groups}}{groupnames to filter on}

\item{\code{active_only}}{Boolean to show if only active accounts need to be shown}

\item{\code{ignore_groups}}{Specific groups to ignore in the result. For example, exclude developer accounts}

\item{\code{by_group}}{Boolean, show by group in result}

\item{\code{add_last_login}}{Boolean, also show when user has logged in last.}
}
\if{html}{\out{</div>}}
}
}
\if{html}{\out{<hr>}}
\if{html}{\out{<a id="method-ShintoUsers-set_user_attributes"></a>}}
\if{latex}{\out{\hypertarget{method-ShintoUsers-set_user_attributes}{}}}
\subsection{Method \code{set_user_attributes()}}{
Set attributes for a user
\subsection{Usage}{
\if{html}{\out{<div class="r">}}\preformatted{shintoUser$set_user_attributes(
  userid,
  appname,
  attributes = list(),
  active = TRUE
)}\if{html}{\out{</div>}}
}

\subsection{Arguments}{
\if{html}{\out{<div class="arguments">}}
\describe{
\item{\code{userid}}{rsconnect username (can be NULL, uses userid on init)}

\item{\code{appname}}{rsconnect application name}

\item{\code{attributes}}{a list}

\item{\code{active}}{Boolean for filtering on certain status}
}
\if{html}{\out{</div>}}
}
}
\if{html}{\out{<hr>}}
\if{html}{\out{<a id="method-ShintoUsers-get_user_attributes"></a>}}
\if{latex}{\out{\hypertarget{method-ShintoUsers-get_user_attributes}{}}}
\subsection{Method \code{get_user_attributes()}}{
Get attributes for a user
\subsection{Usage}{
\if{html}{\out{<div class="r">}}\preformatted{shintoUser$get_user_attributes(userid, appname)}\if{html}{\out{</div>}}
}

\subsection{Arguments}{
\if{html}{\out{<div class="arguments">}}
\describe{
\item{\code{userid}}{rsconnect username (can be NULL, uses userid on init)}

\item{\code{appname}}{rsconnect application name}
}
\if{html}{\out{</div>}}
}
}
\if{html}{\out{<hr>}}
\if{html}{\out{<a id="method-ShintoUsers-get_role"></a>}}
\if{latex}{\out{\hypertarget{method-ShintoUsers-get_role}{}}}
\subsection{Method \code{get_role()}}{
Gets the role for the current user (admin or viewer, typically)
\subsection{Usage}{
\if{html}{\out{<div class="r">}}\preformatted{shintoUser$get_role(userid = NULL, appname = NULL)}\if{html}{\out{</div>}}
}

\subsection{Arguments}{
\if{html}{\out{<div class="arguments">}}
\describe{
\item{\code{userid}}{rsconnect username (can be NULL, uses userid on init)}

\item{\code{appname}}{rsconnect application name}
}
\if{html}{\out{</div>}}
}
}
\if{html}{\out{<hr>}}
\if{html}{\out{<a id="method-ShintoUsers-set_role"></a>}}
\if{latex}{\out{\hypertarget{method-ShintoUsers-set_role}{}}}
\subsection{Method \code{set_role()}}{
Sets the role for a user.
\subsection{Usage}{
\if{html}{\out{<div class="r">}}\preformatted{shintoUser$set_role(userid, appname, role)}\if{html}{\out{</div>}}
}

\subsection{Arguments}{
\if{html}{\out{<div class="arguments">}}
\describe{
\item{\code{userid}}{rsconnect username (can be NULL, uses userid on init)}

\item{\code{appname}}{rsconnect application name}

\item{\code{role}}{The user role, typically 'admin', 'viewer'}
}
\if{html}{\out{</div>}}
}
\subsection{Details}{
!! Do not use in shiny applications (except shintousers_app) !!
}

}
\if{html}{\out{<hr>}}
\if{html}{\out{<a id="method-ShintoUsers-set_user_active_inactive"></a>}}
\if{latex}{\out{\hypertarget{method-ShintoUsers-set_user_active_inactive}{}}}
\subsection{Method \code{set_user_active_inactive()}}{
Make a user active or inactive (sets 'active' field in 'roles' table)
\subsection{Usage}{
\if{html}{\out{<div class="r">}}\preformatted{shintoUser$set_user_active_inactive(
  userid,
  appname,
  what = c("active", "inactive")
)}\if{html}{\out{</div>}}
}

\subsection{Arguments}{
\if{html}{\out{<div class="arguments">}}
\describe{
\item{\code{userid}}{User ID}

\item{\code{appname}}{Application name}

\item{\code{what}}{Either 'active' or 'inactive'}
}
\if{html}{\out{</div>}}
}
}
\if{html}{\out{<hr>}}
\if{html}{\out{<a id="method-ShintoUsers-disable_user"></a>}}
\if{latex}{\out{\hypertarget{method-ShintoUsers-disable_user}{}}}
\subsection{Method \code{disable_user()}}{
Make the user inactive for an app
\subsection{Usage}{
\if{html}{\out{<div class="r">}}\preformatted{shintoUser$disable_user(userid, appname)}\if{html}{\out{</div>}}
}

\subsection{Arguments}{
\if{html}{\out{<div class="arguments">}}
\describe{
\item{\code{userid}}{User ID}

\item{\code{appname}}{Application name}
}
\if{html}{\out{</div>}}
}
}
\if{html}{\out{<hr>}}
\if{html}{\out{<a id="method-ShintoUsers-enable_user"></a>}}
\if{latex}{\out{\hypertarget{method-ShintoUsers-enable_user}{}}}
\subsection{Method \code{enable_user()}}{
Make the user active for an app
\subsection{Usage}{
\if{html}{\out{<div class="r">}}\preformatted{shintoUser$enable_user(userid, appname)}\if{html}{\out{</div>}}
}

\subsection{Arguments}{
\if{html}{\out{<div class="arguments">}}
\describe{
\item{\code{userid}}{User ID}

\item{\code{appname}}{Application name}
}
\if{html}{\out{</div>}}
}
}
\if{html}{\out{<hr>}}
\if{html}{\out{<a id="method-ShintoUsers-get_group"></a>}}
\if{latex}{\out{\hypertarget{method-ShintoUsers-get_group}{}}}
\subsection{Method \code{get_group()}}{
Get groups that the current user belongs to. See also `is_in_group`
\subsection{Usage}{
\if{html}{\out{<div class="r">}}\preformatted{shintoUser$get_group(userid = NULL, appname = NULL)}\if{html}{\out{</div>}}
}

\subsection{Arguments}{
\if{html}{\out{<div class="arguments">}}
\describe{
\item{\code{userid}}{rsconnect username (can be NULL, uses userid on init)}

\item{\code{appname}}{rsconnect application name}
}
\if{html}{\out{</div>}}
}
}
\if{html}{\out{<hr>}}
\if{html}{\out{<a id="method-ShintoUsers-is_in_group"></a>}}
\if{latex}{\out{\hypertarget{method-ShintoUsers-is_in_group}{}}}
\subsection{Method \code{is_in_group()}}{
Is the user in this group? `$is_in_group("superuser")` -> bool
\subsection{Usage}{
\if{html}{\out{<div class="r">}}\preformatted{shintoUser$is_in_group(group)}\if{html}{\out{</div>}}
}

\subsection{Arguments}{
\if{html}{\out{<div class="arguments">}}
\describe{
\item{\code{group}}{Group name}
}
\if{html}{\out{</div>}}
}
}
\if{html}{\out{<hr>}}
\if{html}{\out{<a id="method-ShintoUsers-set_group"></a>}}
\if{latex}{\out{\hypertarget{method-ShintoUsers-set_group}{}}}
\subsection{Method \code{set_group()}}{
Set the group for this user.
\subsection{Usage}{
\if{html}{\out{<div class="r">}}\preformatted{shintoUser$set_group(userid, appname, group)}\if{html}{\out{</div>}}
}

\subsection{Arguments}{
\if{html}{\out{<div class="arguments">}}
\describe{
\item{\code{userid}}{rsconnect username (can be NULL, uses userid on init)}

\item{\code{appname}}{rsconnect application name}

\item{\code{group}}{Group name}
}
\if{html}{\out{</div>}}
}
\subsection{Details}{
!! Do not use in shiny applications (except shintousers_app) !!
}

}
\if{html}{\out{<hr>}}
\if{html}{\out{<a id="method-ShintoUsers-remove_role"></a>}}
\if{latex}{\out{\hypertarget{method-ShintoUsers-remove_role}{}}}
\subsection{Method \code{remove_role()}}{
Remove a role for a user
\subsection{Usage}{
\if{html}{\out{<div class="r">}}\preformatted{shintoUser$remove_role(userid, appname)}\if{html}{\out{</div>}}
}

\subsection{Arguments}{
\if{html}{\out{<div class="arguments">}}
\describe{
\item{\code{userid}}{rsconnect username (can be NULL, uses userid on init)}

\item{\code{appname}}{rsconnect application name}
}
\if{html}{\out{</div>}}
}
\subsection{Details}{
!! Do not use in shiny applications (except shintousers_app) !!
}

}
\if{html}{\out{<hr>}}
\if{html}{\out{<a id="method-ShintoUsers-has_role"></a>}}
\if{latex}{\out{\hypertarget{method-ShintoUsers-has_role}{}}}
\subsection{Method \code{has_role()}}{
Does the current user have a role? `$has_role("admin")` --> bool
\subsection{Usage}{
\if{html}{\out{<div class="r">}}\preformatted{shintoUser$has_role(role)}\if{html}{\out{</div>}}
}

\subsection{Arguments}{
\if{html}{\out{<div class="arguments">}}
\describe{
\item{\code{role}}{The user role, typically 'admin', 'viewer'}
}
\if{html}{\out{</div>}}
}
}
\if{html}{\out{<hr>}}
\if{html}{\out{<a id="method-ShintoUsers-is_admin"></a>}}
\if{latex}{\out{\hypertarget{method-ShintoUsers-is_admin}{}}}
\subsection{Method \code{is_admin()}}{
Better alternative to 'has_role("admin")' for use with AD
\subsection{Usage}{
\if{html}{\out{<div class="r">}}\preformatted{shintoUser$is_admin()}\if{html}{\out{</div>}}
}

}
\if{html}{\out{<hr>}}
\if{html}{\out{<a id="method-ShintoUsers-add_application"></a>}}
\if{latex}{\out{\hypertarget{method-ShintoUsers-add_application}{}}}
\subsection{Method \code{add_application()}}{
Add an application to the list of applications
\subsection{Usage}{
\if{html}{\out{<div class="r">}}\preformatted{shintoUser$add_application(appname, roles, groups = list(), comment = "")}\if{html}{\out{</div>}}
}

\subsection{Arguments}{
\if{html}{\out{<div class="arguments">}}
\describe{
\item{\code{appname}}{The rsconnect application name}

\item{\code{roles}}{The choices for user roles, typically c('admin','viewer')}

\item{\code{groups}}{The choices for application groups, can be anything}

\item{\code{comment}}{Any other text (unused as of 11/2022)}
}
\if{html}{\out{</div>}}
}
\subsection{Details}{
!! Do not use in shiny applications (except shintousers_app) !!
}

}
\if{html}{\out{<hr>}}
\if{html}{\out{<a id="method-ShintoUsers-set_application_roles"></a>}}
\if{latex}{\out{\hypertarget{method-ShintoUsers-set_application_roles}{}}}
\subsection{Method \code{set_application_roles()}}{
Set application available roles
\subsection{Usage}{
\if{html}{\out{<div class="r">}}\preformatted{shintoUser$set_application_roles(appname, roles)}\if{html}{\out{</div>}}
}

\subsection{Arguments}{
\if{html}{\out{<div class="arguments">}}
\describe{
\item{\code{appname}}{The rsconnect application name}

\item{\code{roles}}{The choices for user roles, typically c('admin','viewer')}
}
\if{html}{\out{</div>}}
}
\subsection{Details}{
!! Do not use in shiny applications (except shintousers_app) !!
}

}
\if{html}{\out{<hr>}}
\if{html}{\out{<a id="method-ShintoUsers-set_application_groups"></a>}}
\if{latex}{\out{\hypertarget{method-ShintoUsers-set_application_groups}{}}}
\subsection{Method \code{set_application_groups()}}{
Set groups available in an application
\subsection{Usage}{
\if{html}{\out{<div class="r">}}\preformatted{shintoUser$set_application_groups(appname, groups)}\if{html}{\out{</div>}}
}

\subsection{Arguments}{
\if{html}{\out{<div class="arguments">}}
\describe{
\item{\code{appname}}{The rsconnect application name}

\item{\code{groups}}{The choices for application groups, can be anything}
}
\if{html}{\out{</div>}}
}
\subsection{Details}{
!! Do not use in shiny applications (except shintousers_app) !!
}

}
\if{html}{\out{<hr>}}
\if{html}{\out{<a id="method-ShintoUsers-get_application_roles"></a>}}
\if{latex}{\out{\hypertarget{method-ShintoUsers-get_application_roles}{}}}
\subsection{Method \code{get_application_roles()}}{
Get available roles for an application
\subsection{Usage}{
\if{html}{\out{<div class="r">}}\preformatted{shintoUser$get_application_roles(appname)}\if{html}{\out{</div>}}
}

\subsection{Arguments}{
\if{html}{\out{<div class="arguments">}}
\describe{
\item{\code{appname}}{The rsconnect application name}
}
\if{html}{\out{</div>}}
}
\subsection{Details}{
!! Do not use in shiny applications (except shintousers_app) !!
}

}
\if{html}{\out{<hr>}}
\if{html}{\out{<a id="method-ShintoUsers-get_application_groups"></a>}}
\if{latex}{\out{\hypertarget{method-ShintoUsers-get_application_groups}{}}}
\subsection{Method \code{get_application_groups()}}{
Add an application to the list of applications
\subsection{Usage}{
\if{html}{\out{<div class="r">}}\preformatted{shintoUser$get_application_groups(appname)}\if{html}{\out{</div>}}
}

\subsection{Arguments}{
\if{html}{\out{<div class="arguments">}}
\describe{
\item{\code{appname}}{The rsconnect application name}
}
\if{html}{\out{</div>}}
}
\subsection{Details}{
!! Do not use in shiny applications (except shintousers_app) !!
}

}
\if{html}{\out{<hr>}}
\if{html}{\out{<a id="method-ShintoUsers-get_applications"></a>}}
\if{latex}{\out{\hypertarget{method-ShintoUsers-get_applications}{}}}
\subsection{Method \code{get_applications()}}{
Get list of available applications
\subsection{Usage}{
\if{html}{\out{<div class="r">}}\preformatted{shintoUser$get_applications()}\if{html}{\out{</div>}}
}

\subsection{Details}{
!! Do not use in shiny applications (except shintousers_app) !!
}

}
\if{html}{\out{<hr>}}
\if{html}{\out{<a id="method-ShintoUsers-remove_application"></a>}}
\if{latex}{\out{\hypertarget{method-ShintoUsers-remove_application}{}}}
\subsection{Method \code{remove_application()}}{
Remove an application
\subsection{Usage}{
\if{html}{\out{<div class="r">}}\preformatted{shintoUser$remove_application(appname)}\if{html}{\out{</div>}}
}

\subsection{Arguments}{
\if{html}{\out{<div class="arguments">}}
\describe{
\item{\code{appname}}{The rsconnect application name}
}
\if{html}{\out{</div>}}
}
\subsection{Details}{
!! Do not use in shiny applications (except shintousers_app) !!
}

}
\if{html}{\out{<hr>}}
\if{html}{\out{<a id="method-ShintoUsers-log_timing"></a>}}
\if{latex}{\out{\hypertarget{method-ShintoUsers-log_timing}{}}}
\subsection{Method \code{log_timing()}}{
Log a timing. Writes data to 'timings' table in shintousers, with appname, key and
double precision value.
\subsection{Usage}{
\if{html}{\out{<div class="r">}}\preformatted{shintoUser$log_timing(key, timing)}\if{html}{\out{</div>}}
}

\subsection{Arguments}{
\if{html}{\out{<div class="arguments">}}
\describe{
\item{\code{key}}{Key in database table}

\item{\code{timing}}{Time in seconds}
}
\if{html}{\out{</div>}}
}
}
\if{html}{\out{<hr>}}
\if{html}{\out{<a id="method-ShintoUsers-clone"></a>}}
\if{latex}{\out{\hypertarget{method-ShintoUsers-clone}{}}}
\subsection{Method \code{clone()}}{
The objects of this class are cloneable with this method.
\subsection{Usage}{
\if{html}{\out{<div class="r">}}\preformatted{shintoUser$clone(deep = FALSE)}\if{html}{\out{</div>}}
}

\subsection{Arguments}{
\if{html}{\out{<div class="arguments">}}
\describe{
\item{\code{deep}}{Whether to make a deep clone.}
}
\if{html}{\out{</div>}}
}
}
}
