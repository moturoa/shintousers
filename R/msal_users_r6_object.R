
#' A User object for use in Shinto apps
#' @description Make a database connection to 'shintousers' and access all kinds of
#' methods for user management. In Shiny apps, use `get_and_set_last_login`, for example.
#' @importFrom dplyr collect select filter arrange
#' @importFrom glue glue
#' @importFrom jsonlite toJSON
#' @importFrom shiny getDefaultReactiveDomain isRunning
#' @importFrom R6 R6Class
#' @importFrom shintodb databaseClass
#' @importFrom DBI ANSI sqlInterpolate
#' @importFrom lubridate now
#' @export
shintoMSALUser <- R6::R6Class(classname = "ShintoMSALUser",
                              inherit = shintodb::databaseClass,
                              lock_objects = FALSE,

                              public = list(

                                #' @field con The DB connection to `shintousers`
                                con = NULL,

                                #' @field schema The postgres schema in the DB
                                schema = NULL,

                                #' @field dbusername The DB user name
                                dbusername = NULL,

                                #' @field userid The application (shiny) user name
                                userid = NULL,

                                #' @field appname The  application name
                                appname = NULL,

                                #' @field appversion Version of the app (optionally read from VERSION)
                                appversion = NULL,

                                #' @description Make new shintousers object
                                #' @param userid userid, if not NULL it is stored and used for all methods (handy inside an app)
                                #' @param appname application name
                                #' @param appversion Optional, application version string
                                #' @param ad_groups Optional, active directory-groups
                                #' @param ad_authentication Boolean that checks if active directory authentication has to be done
                                #' @param admin_group_pattern Optional pattern to see which groupnames are implicating admin users
                                #' @param pool Passed to [shintodb::connect()]
                                #' @param dbusername ignored ("shintousers")
                                #' @param dbname ignored ("shintousers")
                                #' @param con Optional, existing database connection to shintousers (for recycling)
                                #' @param ... Further arguments passed to [shintodb::connect()]
                                #' @return A 'shintousers' R6 object
                                initialize = function(userid,
                                                      appname = "",
                                                      appversion = "",
                                                      ad_groups = NULL,
                                                      ad_authentication = FALSE,
                                                      admin_group_pattern = "",
                                                      con = NULL,
                                                      pool = FALSE,
                                                      dbusername = NULL, # ignored
                                                      dbname = NULL,  # ignored

                                                      ...){

                                  if(!is.null(dbusername) | !is.null(dbname)){
                                    message("Arguments 'dbusername' and 'dbname' to shintousers are now ignored! Both should be 'shintousers' in conf/config.yml")
                                  }

                                  super$initialize(what = "shintousers", schema = "shintousers", db_connection = con, pool = pool, ...)

                                  if(is.null(userid) || trimws(userid) == ""){
                                    message("The input userid is NULL. Stopping execution.")
                                    stop("Function terminated because userid is NULL.")
                                  }
                                  if(is.null(appname) || trimws(appname) == ""){
                                    message("The input appname is NULL. Stopping execution.")
                                    stop("Function terminated because appname is NULL.")
                                  }

                                  self$userid <- userid
                                  self$appname <- appname


                                  self$appversion <- appversion

                                  self$ad_groups <- ad_groups
                                  self$ad_authentication <- ad_authentication
                                  self$admin_group_pattern <- admin_group_pattern

                                },

                                #' @description Convert to JSON
                                #' @param x An object to convert to JSON
                                to_json = function(x){
                                  jsonlite::toJSON(x)
                                },

                                #' @description Convert to JSON
                                #' @param txt Text string, JSON
                                from_json = function(txt){
                                  jsonlite::fromJSON(txt)
                                },

                                #' @description Add user to msal registry in shintousers
                                #' @param userid userid from MSAL for user
                                #' @param appname appname for the application user has access to
                                #' @param role Roles user has within application
                                #' @param groups JSON with groups user belongs to within application
                                #' @param username username (or label) for user
                                #' @param email email from user
                                #' @param attributes attributes from user
                                #' @param comments comments about user
                                add_msal_user =  function(userid, appname, role = NA, groups = NA,
                                                          username = NA, email, attributes = NA, comments = NA){

                                  if(is.null(userid) || trimws(userid) == ""){
                                    message("The input userid is NULL. Stopping execution.")
                                    stop("Function terminated because userid is NULL.")
                                  }
                                  if(is.null(appname) || trimws(appname) == ""){
                                    message("The input appname is NULL. Stopping execution.")
                                    stop("Function terminated because appname is NULL.")
                                  }
                                  if(is.null(email) || trimws(email) == ""){
                                    message("The input email is NULL. Stopping execution.")
                                    stop("Function terminated because email is NULL.")
                                  }


                                  qu <- glue::glue("INSERT INTO {self$schema}.shiny_msal_accounts (userid, appname, role, groups, username, email, attributes, comments) VALUES(?userid, ?appname, ?role, ?groups, ?username, ?email, ?attributes, ?comments)") %>% as.character()


                                  query <- DBI::sqlInterpolate(DBI::ANSI(),
                                                               qu,
                                                               userid = userid,
                                                               appname = appname,
                                                               role = role,
                                                               groups = groups,
                                                               username = username,
                                                               email = email,
                                                               attributes = attributes,
                                                               comments = comments)

                                  self$execute_query(query)

                                },

                                #' @description Read username for a user (default = current user)
                                #' @param userid Vector of user ID's
                                #' @param appname Application name
                                get_name = function(userid, appname){

                                  if(is.null(userid) || all(trimws(userid) == "")){
                                    message("The input userid is NULL. Stopping execution.")
                                    stop("Function terminated because userid is NULL.")
                                  }
                                  if(is.null(appname) || trimws(appname) == ""){
                                    message("The input appname is NULL. Stopping execution.")
                                    stop("Function terminated because appname is NULL.")
                                  }

                                  data <- self$read_table("shiny_msal_accounts", lazy = TRUE) |>
                                    dplyr::filter(appname == !!appname) |>
                                    dplyr::filter(userid %in% !!userid) |>
                                    dplyr::select(username) |>
                                    dplyr::collect() |>
                                    dplyr::pull()

                                  if(length(data) == 0 || is.null(data) || all(is.na(data))){
                                    return(NULL)
                                  } else {
                                    return(data)
                                  }


                                },

                                #' @description Sets the username for a user.
                                #' @param userid msal userid
                                #' @param appname shiny application name
                                #' @param username The username, typically Voornaam Achternaam
                                set_user_name = function(userid, appname, username){

                                  if(is.null(userid) || trimws(userid) == ""){
                                    message("The input userid is NULL. Stopping execution.")
                                    stop("Function terminated because userid is NULL.")
                                  }
                                  if(is.null(appname) || trimws(appname) == ""){
                                    message("The input appname is NULL. Stopping execution.")
                                    stop("Function terminated because appname is NULL.")
                                  }

                                  if(!self$app_has_user(userid, appname)){

                                    message("Tried to set the username for a non-existent user")
                                    return(NULL)

                                  } else {

                                    qu <- glue::glue("UPDATE {self$schema}.shiny_msal_accounts SET username = ?username
                                 WHERE userid = ?userid and appname = ?appname") %>% as.character()

                                    query <- DBI::sqlInterpolate(DBI::ANSI(),
                                                                 qu,
                                                                 username = username,
                                                                 userid = userid,
                                                                 appname = appname)

                                    self$execute_query(query)

                                  }

                                },


                                #' @description Read email address for a user (default = current user)
                                #' @param userid Vector of user ID's
                                #' @param appname Application name (can be NULL, appname on init is then used)
                                get_email = function(userid, appname){

                                  if(is.null(userid) || trimws(userid) == ""){
                                    message("The input userid is NULL. Stopping execution.")
                                    stop("Function terminated because userid is NULL.")
                                  }
                                  if(is.null(appname) || trimws(appname) == ""){
                                    message("The input appname is NULL. Stopping execution.")
                                    stop("Function terminated because appname is NULL.")
                                  }

                                  data <- self$read_table("shiny_msal_accounts", lazy = TRUE) |>
                                    dplyr::filter(appname == !!appname) |>
                                    dplyr::filter(userid %in% !!userid) |>
                                    dplyr::select(email) |>
                                    dplyr::collect() |>
                                    dplyr::pull()

                                  if(length(data) == 0 || is.null(data) || is.na(data)){
                                    return(NULL)
                                  } else {
                                    return(data)
                                  }


                                },

                                #' @description Sets the email for a user.
                                #' @param userid msal userid
                                #' @param appname shiny application name
                                #' @param email The email
                                set_user_email = function(userid, appname, email){

                                  if(is.null(userid) || trimws(userid) == ""){
                                    message("The input userid is NULL. Stopping execution.")
                                    stop("Function terminated because userid is NULL.")
                                  }
                                  if(is.null(appname) || trimws(appname) == ""){
                                    message("The input appname is NULL. Stopping execution.")
                                    stop("Function terminated because appname is NULL.")
                                  }
                                  if(is.null(email) || trimws(email) == ""){
                                    message("The input email is NULL. Stopping execution.")
                                    stop("Function terminated because email is NULL.")
                                  }

                                  if(!self$app_has_user(userid, appname)){

                                    message("Tried to set the email for a non-existent user")
                                    return(NULL)

                                  } else {
                                    qu <- glue::glue("UPDATE {self$schema}.shiny_msal_accounts SET email = ?email
                                 WHERE userid = ?userid and appname = ?appname") %>% as.character()

                                    query <- DBI::sqlInterpolate(DBI::ANSI(),
                                                                 qu,
                                                                 email = email,
                                                                 userid = userid,
                                                                 appname = appname)

                                    self$execute_query(query)

                                  }

                                },

                                #' @description Read comment for a user (default = current user)
                                #' @param userid Vector of user ID's
                                #' @param appname Application name (can be NULL, appname on init is then used)
                                get_comment = function(userid, appname){

                                  if(is.null(userid) || trimws(userid) == ""){
                                    message("The input userid is NULL. Stopping execution.")
                                    stop("Function terminated because userid is NULL.")
                                  }
                                  if(is.null(appname) || trimws(appname) == ""){
                                    message("The input appname is NULL. Stopping execution.")
                                    stop("Function terminated because appname is NULL.")
                                  }

                                  data <- self$read_table("shiny_msal_accounts", lazy = TRUE) |>
                                    dplyr::filter(appname == !!appname) |>
                                    dplyr::filter(userid %in% !!userid) |>
                                    dplyr::select(comments) |>
                                    dplyr::collect() |>
                                    dplyr::pull()

                                  if(length(data) == 0 || is.null(data) || is.na(data)){
                                    return(NULL)
                                  } else {
                                    return(data)
                                  }


                                },

                                #' @description Sets the comment for a user.
                                #' @param userid msal userid
                                #' @param appname application name
                                #' @param comments The comments
                                set_comment = function(userid, appname, comments){

                                  if(is.null(userid) || trimws(userid) == ""){
                                    message("The input userid is NULL. Stopping execution.")
                                    stop("Function terminated because userid is NULL.")
                                  }
                                  if(is.null(appname) || trimws(appname) == ""){
                                    message("The input appname is NULL. Stopping execution.")
                                    stop("Function terminated because appname is NULL.")
                                  }

                                  if(!self$app_has_user(userid, appname)){

                                    message("Tried to set the comment for a non-existent user")
                                    return(NULL)

                                  } else {

                                    qu <- glue::glue("UPDATE {self$schema}.shiny_msal_accounts SET comments = ?comments
                                 WHERE userid = ?userid and appname = ?appname") %>% as.character()

                                    query <- DBI::sqlInterpolate(DBI::ANSI(),
                                                                 qu,
                                                                 comments = comments,
                                                                 userid = userid,
                                                                 appname = appname)

                                    self$execute_query(query)

                                  }

                                },



                                #' @description Get last login for this user for this application (reads `shintousers.logins`)
                                #' @param userid msal userid
                                #' @param appname application name
                                get_last_login = function(userid, appname){

                                  if(is.null(userid) || trimws(userid) == ""){
                                    message("The input userid is NULL. Stopping execution.")
                                    stop("Function terminated because userid is NULL.")
                                  }
                                  if(is.null(appname) || trimws(appname) == ""){
                                    message("The input appname is NULL. Stopping execution.")
                                    stop("Function terminated because appname is NULL.")
                                  }

                                  qu <- glue::glue("SELECT timestamp, appversion FROM {self$schema}.logins WHERE
                                  userid = ?userid and appname = ?appname") %>% as.character()

                                  query <- DBI::sqlInterpolate(DBI::ANSI(),
                                                               qu,
                                                               userid = userid,
                                                               appname = appname)

                                  out <- self$query(query)



                                  if(nrow(out) == 0)return(NULL)
                                  setNames(as.POSIXct(out$timestamp, tz = "UTC"), out$appversion)
                                },

                                #' @description Update the last login for this user / appname (in table `shintousers.logins`)
                                #' @param now Optional, time string (defaults to sys time)
                                #' @param userid msal userid
                                #' @param appname application name
                                #' @param appversion application version string
                                set_last_login = function(now = as.character(Sys.time()),
                                                          userid, appname, appversion = NULL){

                                  if(is.null(userid) || trimws(userid) == ""){
                                    message("The input userid is NULL. Stopping execution.")
                                    stop("Function terminated because userid is NULL.")
                                  }
                                  if(is.null(appname) || trimws(appname) == ""){
                                    message("The input appname is NULL. Stopping execution.")
                                    stop("Function terminated because appname is NULL.")
                                  }
                                  if(is.null(appversion) || trimws(appversion) == ""){
                                    message("The input appversion is NULL. Stopping execution.")
                                    stop("Function terminated because appversion is NULL.")
                                  }


                                  qu <- glue::glue("UPDATE {self$schema}.logins SET timestamp = ?now, appversion = ?appversion ",
                                                   "WHERE userid = ?userid and appname = ?appname") %>% as.character()

                                  query <- DBI::sqlInterpolate(DBI::ANSI(),
                                                               qu,
                                                               now = now,
                                                               appversion = appversion,
                                                               userid = userid,
                                                               appname = appname)

                                  self$execute_query(query)

                                },

                                #' @description Reads last login, and sets the current time as the new 'last login'.
                                #' @details If user has never logged in, writes a new line in `shintousers.logins`,
                                #' otherwise updates the last login.
                                #' @returns Returns (invisibly) the last login information
                                #' @param userid msal userid
                                #' @param appname application name
                                #' @param appversion application version string
                                get_and_set_last_login = function(userid, appname, appversion = NULL){

                                  if(is.null(appversion))appversion <- self$appversion

                                  if(is.null(userid) || trimws(userid) == ""){
                                    message("The input userid is NULL. Stopping execution.")
                                    stop("Function terminated because userid is NULL.")
                                  }
                                  if(is.null(appname) || trimws(appname) == ""){
                                    message("The input appname is NULL. Stopping execution.")
                                    stop("Function terminated because appname is NULL.")
                                  }
                                  if(is.null(appversion) || trimws(appversion) == ""){
                                    message("The input appversion is NULL. Stopping execution.")
                                    stop("Function terminated because appversion is NULL.")
                                  }

                                  user_log <- self$get_last_login(userid = userid, appname = appname)

                                  # User has not previously logged in
                                  if(is.null(user_log)){
                                    qu <- glue::glue("INSERT INTO {self$schema}.logins (timestamp, userid, appname, appversion) VALUES(?timestamp, ?userid, ?appname, ?appversion)") %>% as.character()


                                    query <- DBI::sqlInterpolate(DBI::ANSI(),
                                                                 qu,
                                                                 timestamp  = as.character(Sys.time()),
                                                                 userid = userid,
                                                                 appname = appname,
                                                                 appversion = appversion)

                                    self$execute_query(query)

                                    user_log <- self$get_last_login(userid = userid, appname = appname)
                                  }

                                  self$set_last_login(userid = userid, appname = appname, appversion = appversion)

                                  return(invisible(user_log))
                                },

                                #' @description Does an app have a user configured?
                                #' @param userid msal userid
                                #' @param appname application name
                                app_has_user = function(userid, appname){

                                  if(is.null(userid) || trimws(userid) == ""){
                                    message("The input userid is NULL. Stopping execution.")
                                    stop("Function terminated because userid is NULL.")
                                  }
                                  if(is.null(appname) || trimws(appname) == ""){
                                    message("The input appname is NULL. Stopping execution.")
                                    stop("Function terminated because appname is NULL.")
                                  }

                                  qu <- glue::glue("select * from {self$schema}.shiny_msal_accounts
                                                   where userid = ?userid and appname = ?appname") %>% as.character()


                                  query <- DBI::sqlInterpolate(DBI::ANSI(),
                                                               qu,
                                                               userid = userid,
                                                               appname = appname)

                                  out <- self$query(query)

                                  nrow(out) > 0

                                },

                                #' @description List users for an app
                                #' @param appname application name
                                #' @param roles rolnames to filter on
                                #' @param groups groupnames to filter on
                                #' @param active_only Boolean to show if only active accounts need to be shown
                                #' @param ignore_groups Specific groups to ignore in the result. For example, exclude developer accounts
                                #' @param by_group Boolean, show by group in result
                                #' @param add_last_login Boolean, also show when user has logged in last.
                                list_application_users = function(appname,
                                                                  roles = NULL,
                                                                  groups = NULL,
                                                                  active_only = TRUE,
                                                                  ignore_groups = NULL,
                                                                  by_group = FALSE,
                                                                  add_last_login = FALSE){


                                  if(is.null(appname) || trimws(appname) == ""){
                                    message("The input appname is NULL. Stopping execution.")
                                    stop("Function terminated because appname is NULL.")
                                  }

                                  data <- self$read_table("shiny_msal_accounts", lazy = TRUE) |>
                                    dplyr::filter(appname == !!appname) |>
                                    dplyr::collect()

                                  data <- dplyr::arrange(data, username) |>
                                    dplyr::select(userid, username, groups, role, active)

                                  if(active_only){
                                    data <- dplyr::filter(data, active)
                                  }

                                  if(!is.null(roles)){
                                    data <- dplyr::filter(data, role %in% !!roles)
                                  }

                                  if(!is.null(groups)){
                                    data <- dplyr::filter(data,
                                                          grepl(paste(!!groups,collapse="|"), groups))
                                  }

                                  if(!is.null(ignore_groups)){

                                    data <- dplyr::filter(data,
                                                          !grepl(paste(ignore_groups,collapse="|"), groups))

                                  }

                                  if(add_last_login){

                                    last_logins <- self$read_table("logins", lazy = TRUE) |>
                                      dplyr::filter(appname == !!appname) |>
                                      dplyr::select(userid, last_login = timestamp) |>
                                      dplyr::collect() |>
                                      dplyr::distinct(userid, .keep_all = TRUE)

                                    data <- dplyr::left_join(data, last_logins, by = "userid")

                                  }


                                  if(by_group){

                                    all_groups <- unique(do.call(c, lapply(data$groups, self$from_json)))

                                    data <- lapply(all_groups, function(g){
                                      dplyr::filter(data, grepl(g, groups))
                                    })

                                    names(data) <- all_groups

                                  }

                                  return(data)

                                },

                                #' @description Set attributes for a user
                                #' @param userid msal userid
                                #' @param appname application name
                                #' @param attributes a list
                                set_user_attributes = function(userid, appname, attributes = list()){

                                  if(is.null(userid) || trimws(userid) == ""){
                                    message("The input userid is NULL. Stopping execution.")
                                    stop("Function terminated because userid is NULL.")
                                  }
                                  if(is.null(appname) || trimws(appname) == ""){
                                    message("The input appname is NULL. Stopping execution.")
                                    stop("Function terminated because appname is NULL.")
                                  }

                                  atr_json <- self$to_json(attributes)

                                  if(!self$app_has_user(userid, appname)){

                                    message("Tried to set user attrabutes for a non-existent user")
                                    return(NULL)

                                  } else {

                                    qu <- glue::glue("UPDATE {self$schema}.shiny_msal_accounts SET attributes = ?atr_json",
                                                     " WHERE userid = ?userid and appname = ?appname") %>% as.character()

                                    query <- DBI::sqlInterpolate(DBI::ANSI(),
                                                                 qu,
                                                                 atr_json = atr_json,
                                                                 userid = userid,
                                                                 appname = appname)

                                    self$execute_query(query)

                                  }

                                },

                                #' @description Get attributes for a user
                                #' @param userid msal userid
                                #' @param appname application name
                                get_user_attributes = function(userid, appname){

                                  if(is.null(userid) || trimws(userid) == ""){
                                    message("The input userid is NULL. Stopping execution.")
                                    stop("Function terminated because userid is NULL.")
                                  }
                                  if(is.null(appname) || trimws(appname) == ""){
                                    message("The input appname is NULL. Stopping execution.")
                                    stop("Function terminated because appname is NULL.")
                                  }

                                  out <- self$read_table("shiny_msal_accounts", lazy = TRUE) |>
                                    dplyr::filter(userid %in% !!userid, appname == !!appname) |>
                                    dplyr::select(attributes) |>
                                    dplyr::collect() |>
                                    dplyr::pull()

                                  if(length(out) == 0 || is.null(out) || is.na(out)){
                                    return(NULL)
                                  } else {
                                    return(out)
                                  }
                                },

                                #' @description Gets the role for the current user (admin or viewer, typically)
                                #' @param userid msal userid
                                #' @param appname application name
                                get_role = function(userid, appname){

                                  if(is.null(userid) || trimws(userid) == ""){
                                    message("The input userid is NULL. Stopping execution.")
                                    stop("Function terminated because userid is NULL.")
                                  }
                                  if(is.null(appname) || trimws(appname) == ""){
                                    message("The input appname is NULL. Stopping execution.")
                                    stop("Function terminated because appname is NULL.")
                                  }

                                  qu <- glue::glue("select role from {self$schema}.shiny_msal_accounts where userid = ?userid and appname = ?appname") %>% as.character()


                                  query <- DBI::sqlInterpolate(DBI::ANSI(),
                                                               qu,
                                                               userid = userid,
                                                               appname = appname)

                                  out <- self$query(query)

                                  if(nrow(out) == 0){
                                    return(NULL)
                                  }

                                  out$role

                                },

                                #' @description Sets the role for a user.
                                #' @param userid msal userid
                                #' @param appname application name
                                #' @param role The user role, typically 'admin', 'viewer'
                                set_role = function(userid, appname, role){

                                  if(is.null(userid) || trimws(userid) == ""){
                                    message("The input userid is NULL. Stopping execution.")
                                    stop("Function terminated because userid is NULL.")
                                  }
                                  if(is.null(appname) || trimws(appname) == ""){
                                    message("The input appname is NULL. Stopping execution.")
                                    stop("Function terminated because appname is NULL.")
                                  }

                                  if(!self$app_has_user(userid, appname)){

                                    message("Tried to set role for a non-existent user")
                                    return(NULL)

                                  } else {

                                    qu <- glue::glue("UPDATE {self$schema}.shiny_msal_accounts SET role = ?role
                                 WHERE userid = ?userid and appname = ?appname") %>% as.character()

                                    query <- DBI::sqlInterpolate(DBI::ANSI(),
                                                                 qu,
                                                                 role = role,
                                                                 userid = userid,
                                                                 appname = appname)

                                    self$execute_query(query)

                                  }

                                },

                                #' @description Gets the activity status of a user
                                #' @param userid msal userid
                                #' @param appname application name
                                get_user_active_inactive = function(userid, appname){

                                  if(is.null(userid) || trimws(userid) == ""){
                                    message("The input userid is NULL. Stopping execution.")
                                    stop("Function terminated because userid is NULL.")
                                  }
                                  if(is.null(appname) || trimws(appname) == ""){
                                    message("The input appname is NULL. Stopping execution.")
                                    stop("Function terminated because appname is NULL.")
                                  }

                                  qu <- glue::glue("select active from {self$schema}.shiny_msal_accounts where userid = ?userid and appname = ?appname") %>% as.character()


                                  query <- DBI::sqlInterpolate(DBI::ANSI(),
                                                               qu,
                                                               userid = userid,
                                                               appname = appname)

                                  out <- self$query(query)

                                  if(nrow(out) == 0){
                                    return(NULL)
                                  }

                                  out$active

                                },

                                #' @description Make a user active or inactive (sets 'active' field in 'roles' table)
                                #' @param userid User ID
                                #' @param appname Application name
                                #' @param what Either 'active' or 'inactive'
                                set_user_active_inactive = function(userid, appname, what = c("active","inactive")){

                                  if(is.null(userid) || trimws(userid) == ""){
                                    message("The input userid is NULL. Stopping execution.")
                                    stop("Function terminated because userid is NULL.")
                                  }
                                  if(is.null(appname) || trimws(appname) == ""){
                                    message("The input appname is NULL. Stopping execution.")
                                    stop("Function terminated because appname is NULL.")
                                  }

                                  what <- match.arg(what)
                                  val <- what == "active"
                                  val <- tolower(val)

                                  if(!self$app_has_user(userid, appname)){

                                    message("Tried to enable/disable a non-existent user")
                                    return(NULL)

                                  } else {

                                    qu <- glue::glue("UPDATE {self$schema}.shiny_msal_accounts SET active = ?val
                                 WHERE userid = ?userid and appname = ?appname") %>% as.character()

                                    query <- DBI::sqlInterpolate(DBI::ANSI(),
                                                                 qu,
                                                                 val = val,
                                                                 userid = userid,
                                                                 appname = appname)

                                    self$execute_query(query)


                                  }

                                },

                                #' @description Make the user inactive for an app
                                #' @param userid User ID
                                #' @param appname Application name
                                disable_user = function(userid, appname){

                                  if(is.null(userid) || trimws(userid) == ""){
                                    message("The input userid is NULL. Stopping execution.")
                                    stop("Function terminated because userid is NULL.")
                                  }
                                  if(is.null(appname) || trimws(appname) == ""){
                                    message("The input appname is NULL. Stopping execution.")
                                    stop("Function terminated because appname is NULL.")
                                  }

                                  self$set_user_active_inactive(userid, appname, "inactive")

                                },

                                #' @description Make the user active for an app
                                #' @param userid User ID
                                #' @param appname Application name
                                enable_user = function(userid, appname){

                                  if(is.null(userid) || trimws(userid) == ""){
                                    message("The input userid is NULL. Stopping execution.")
                                    stop("Function terminated because userid is NULL.")
                                  }
                                  if(is.null(appname) || trimws(appname) == ""){
                                    message("The input appname is NULL. Stopping execution.")
                                    stop("Function terminated because appname is NULL.")
                                  }

                                  self$set_user_active_inactive(userid, appname, "active")

                                },

                                #' @description Get groups that the current user belongs to. See also `is_in_group`
                                #' @param userid msal userid
                                #' @param appname application name
                                get_group = function(userid, appname){

                                  if(is.null(userid) || trimws(userid) == ""){
                                    message("The input userid is NULL. Stopping execution.")
                                    stop("Function terminated because userid is NULL.")
                                  }
                                  if(is.null(appname) || trimws(appname) == ""){
                                    message("The input appname is NULL. Stopping execution.")
                                    stop("Function terminated because appname is NULL.")
                                  }

                                  qu <- glue::glue("select groups from {self$schema}.shiny_msal_accounts where userid = ?userid and appname = ?appname") %>% as.character()


                                  query <- DBI::sqlInterpolate(DBI::ANSI(),
                                                               qu,
                                                               userid = userid,
                                                               appname = appname)

                                  out <- self$query(query)

                                  if(nrow(out) == 0 || is.na(out$groups[1]) || out$groups == ""){
                                    return(NULL)
                                  }

                                  jsonlite::fromJSON(out$groups)

                                },

                                #' @description Is the user in this group? `$is_in_group("superuser")` -> bool
                                #' @param group Group name
                                is_in_group = function(group){

                                  isTRUE(group %in% self$get_group())

                                },

                                #' @description Set the group for this user.
                                #' @param group Group name
                                #' @param userid msal userid
                                #' @param appname application name
                                set_group = function(userid, appname, group){

                                  # In previous version if NULL, the set_group was ignored.
                                  # This however caused the removal of all groups to be impossible
                                  # The statement has been commented for now in case something breaks

                                  if(is.null(userid) || trimws(userid) == ""){
                                    message("The input userid is NULL. Stopping execution.")
                                    stop("Function terminated because userid is NULL.")
                                  }
                                  if(is.null(appname) || trimws(appname) == ""){
                                    message("The input appname is NULL. Stopping execution.")
                                    stop("Function terminated because appname is NULL.")
                                  }

                                  if(is.null(group)){
                                    group <- ""
                                  } else {
                                    group <- self$to_json(group)
                                  }

                                  if(!self$app_has_user(userid, appname)){

                                    message("Tried to set group(s) for a non-existent user")
                                    return(NULL)

                                  } else {

                                    qu <- glue::glue("UPDATE {self$schema}.shiny_msal_accounts SET groups = ?groups
                                 WHERE userid = ?userid and appname = ?appname") %>% as.character()

                                    query <- DBI::sqlInterpolate(DBI::ANSI(),
                                                                 qu,
                                                                 groups = group,
                                                                 userid = userid,
                                                                 appname = appname)

                                    self$execute_query(query)


                                  }

                                },

                                #' @description Remove a role for a user
                                #' @param userid msal userid
                                #' @param appname  application name
                                remove_msal_account_from_app = function(userid, appname){

                                  if(is.null(userid) || trimws(userid) == ""){
                                    message("The input userid is NULL. Stopping execution.")
                                    stop("Function terminated because userid is NULL.")
                                  }
                                  if(is.null(appname) || trimws(appname) == ""){
                                    message("The input appname is NULL. Stopping execution.")
                                    stop("Function terminated because appname is NULL.")
                                  }

                                  qu <- glue::glue("delete from {self$schema}.shiny_msal_accounts where userid = ?userid and appname = ?appname") %>% as.character()

                                  query <- DBI::sqlInterpolate(DBI::ANSI(),
                                                               qu,
                                                               userid = userid,
                                                               appname = appname)

                                  self$execute_query(query)

                                },

                                #' @description Does the current user have a role? `$has_role("admin")` --> bool
                                #' @param role The user role, typically 'admin', 'viewer'
                                #' @param userid msal userid
                                #' @param appname  application name
                                has_role = function(role, userid, appname){
                                  if(is.null(userid) || trimws(userid) == ""){
                                    message("The input userid is NULL. Stopping execution.")
                                    stop("Function terminated because userid is NULL.")
                                  }
                                  if(is.null(appname) || trimws(appname) == ""){
                                    message("The input appname is NULL. Stopping execution.")
                                    stop("Function terminated because appname is NULL.")
                                  }

                                  if(role == "admin" && self$ad_authentication){
                                    self$is_admin()
                                  } else {
                                    roles <- self$get_role(userid = userid, appname = appname)
                                    isTRUE(as.character(role) %in% as.character(roles))
                                  }

                                },

                                #' @description Better alternative to 'has_role("admin")' for use with AD
                                is_admin = function(){

                                  if(!self$ad_authentication){
                                    self$has_role("admin")
                                  } else {
                                    # search for admin group pattern (e.g. 'Beheerders$')
                                    pat <- paste(self$admin_group_pattern, collapse="|")
                                    any(grepl(pat, self$ad_groups))
                                  }

                                },



                                #' @description Get available roles for an application
                                #' @details !! Do not use in shiny applications (except shintousers_app) !!
                                #' @param appname The application name
                                get_application_roles = function(appname){

                                  if(is.null(appname) || trimws(appname) == ""){
                                    message("The input appname is NULL. Stopping execution.")
                                    stop("Function terminated because appname is NULL.")
                                  }

                                  qu <- glue::glue("select roles from {self$schema}.applications where appname = ?appname") %>% as.character()


                                  query <- DBI::sqlInterpolate(DBI::ANSI(),
                                                               qu,
                                                               appname = appname)

                                  out <- self$query(query)$roles

                                  if(all(is.na(out)) || length(out) == 0){
                                    return(NA)
                                  } else {
                                    return(self$from_json(out))
                                  }

                                },

                                #' @description Add an application to the list of applications
                                #' @details !! Do not use in shiny applications (except shintousers_app) !!
                                #' @param appname The application name
                                get_application_groups = function(appname){

                                  if(is.null(appname) || trimws(appname) == ""){
                                    message("The input appname is NULL. Stopping execution.")
                                    stop("Function terminated because appname is NULL.")
                                  }

                                  qu <- glue::glue("select groups from {self$schema}.applications where appname = ?appname") %>% as.character()


                                  query <- DBI::sqlInterpolate(DBI::ANSI(),
                                                               qu,
                                                               appname = appname)

                                  out <- self$query(query)$groups


                                  if(all(is.na(out)) || length(out) == 0 || out == ""){
                                    return(NA)
                                  } else {
                                    return(self$from_json(out))
                                  }

                                },

                                #' @description Log a timing. Writes data to 'timings' table in shintousers, with appname, key and
                                #' double precision value.
                                #' @param key Key in database table
                                #' @param timing Time in seconds
                                log_timing = function(key, timing){

                                  v_n <- try(as.numeric(timing))
                                  if(inherits(v_n, "try-error")){
                                    message("non-numeric to $log_timing")
                                    return(NULL)
                                  }


                                  qu <- glue::glue("INSERT INTO {self$schema}.timings (appname, key, value, userid) VALUES(?appname, ?key, ?value, ?userid)") %>% as.character()


                                  query <- DBI::sqlInterpolate(DBI::ANSI(),
                                                               qu,
                                                               appname = self$appname,
                                                               key = key,
                                                               value = v_n,
                                                               userid  = self$userid)

                                  self$execute_query(query)



                                },


                                ##### Invitations #####

                                #' @description Add an invitation for a user
                                #' @param email email of the user to be invited
                                #' @param username name of the user
                                #' @param invite_sent_by name of user that sent invite
                                #' @param appname appname of the application the invite is sent for
                                #' @param role the roles the invitee should get upon accepting
                                #' @param groups the groups the invitee should belong to upon accepting
                                #' @param expire_date The date on which the invite expires
                                add_invite = function(email, username = NULL, invite_sent_by, appname, role = NA, groups = NULL){

                                  if(is.null(appname) || trimws(appname) == ""){
                                    message("The input appname is NULL. Stopping execution.")
                                    stop("Function terminated because appname is NULL.")
                                  }
                                  if(is.null(email) || trimws(email) == ""){
                                    message("The input email is NULL. Stopping execution.")
                                    stop("Function terminated because email is NULL.")
                                  }
                                  if(is.null(invite_sent_by) || trimws(invite_sent_by) == ""){
                                    message("The input invite_sent_by is NULL. Stopping execution.")
                                    stop("Function terminated because invite_sent_by is NULL.")
                                  }

                                  email_lowered <- tolower(email)

                                  if(is.null(groups)){
                                    parsed_groups <- ""
                                  } else {
                                    parsed_groups <- self$to_json(groups)
                                  }

                                  qu <- glue::glue("INSERT INTO {self$schema}.shiny_msal_accounts_pending_invites (email, username, invite_sent_by, appname, role, groups) VALUES(?email, ?username, ?invite_sent_by, ?appname, ?role, ?parsed_groups)") %>% as.character()


                                  query <- DBI::sqlInterpolate(DBI::ANSI(),
                                                               qu,
                                                               email = email_lowered,
                                                               username = username,
                                                               invite_sent_by = invite_sent_by,
                                                               appname = appname,
                                                               role = role,
                                                               parsed_groups = parsed_groups)

                                  self$execute_query(query)

                                },

                                #' @description Checks if there is an invite connected to email address for appname
                                #' @param appname application name
                                #' @param email email address of user to be checked
                                #' @param ignore_expiration_date Boolean that tells if a filter should be applied based on expiration date. Handy to find if any invite exits or a to find invites that are still valid
                                has_invite = function(appname, email, ignore_expiration_date = FALSE){
                                  if(is.null(appname) || trimws(appname) == ""){
                                    message("The input appname is NULL. Stopping execution.")
                                    stop("Function terminated because appname is NULL.")
                                  }
                                  if(is.null(email) || trimws(email) == ""){
                                    message("The input email is NULL. Stopping execution.")
                                    stop("Function terminated because email is NULL.")
                                  }

                                  email_lowered <- tolower(email)

                                  qu <- glue::glue("select * from {self$schema}.shiny_msal_accounts_pending_invites where appname = ?appname AND email = ?email") %>% as.character()


                                  query <- DBI::sqlInterpolate(DBI::ANSI(),
                                                               qu,
                                                               appname = appname,
                                                               email = email_lowered)

                                  out <- self$query(query)

                                  if(ignore_expiration_date){
                                    out <- out
                                  } else {
                                    out <- out %>%
                                      dplyr::filter(lubridate::now() <= expire_date)
                                  }

                                  if(nrow(out) == 1){
                                    return(TRUE)
                                  } else if(nrow(out) == 0){
                                    return(FALSE)
                                  } else {
                                    message("Impossible amount of invites for user; can only be zero or one.")
                                    stop("Function terminated because something is wrong with invites for this person")
                                  }

                                },

                                #' @description Checks if there is an invite connected to email adress for appname
                                #' @param appname application name
                                #' @param email email address of user to be checked
                                get_invite = function(appname, email){

                                  if(is.null(appname) || trimws(appname) == ""){
                                    message("The input appname is NULL. Stopping execution.")
                                    stop("Function terminated because appname is NULL.")
                                  }
                                  if(is.null(email) || trimws(email) == ""){
                                    message("The input email is NULL. Stopping execution.")
                                    stop("Function terminated because email is NULL.")
                                  }

                                  email_lowered <- tolower(email)

                                  qu <- glue::glue("select * from {self$schema}.shiny_msal_accounts_pending_invites where appname = ?appname AND email = ?email") %>% as.character()


                                  query <- DBI::sqlInterpolate(DBI::ANSI(),
                                                               qu,
                                                               appname = appname,
                                                               email = email_lowered)

                                  out <- self$query(query)

                                  if(nrow(unique(out)) > 1){
                                    message("Multiple different invites for user sent. No way of knowing which one is correct. Please check.")
                                    stop("Function terminated because impossible amount of invites for user is present")
                                  } else {
                                    return(unique(out))
                                  }

                                },

                                #' @description Update an invitation for a user
                                #' @param invite_id The id of the invitation in de the table shiny_msal_accounts_pending_invites
                                #' @param email email of the user to be invited
                                #' @param username name of the user
                                #' @param invite_sent_by name of user that sent invite
                                #' @param appname appname of the application the invite is sent for
                                #' @param role the roles the invitee should get upon accepting
                                #' @param groups the groups the invitee should belong to upon accepting
                                #' @param expire_date The date on which the invite expires
                                update_invite = function(invite_id, email, username = NULL, invite_sent_by, appname, role = NA, groups = NULL){

                                  if(is.null(invite_id) || trimws(invite_id) == ""){
                                    message("The input invite_id is NULL. Stopping execution.")
                                    stop("Function terminated because invite_id is NULL.")
                                  }
                                  if(is.null(appname) || trimws(appname) == ""){
                                    message("The input appname is NULL. Stopping execution.")
                                    stop("Function terminated because appname is NULL.")
                                  }
                                  if(is.null(email) || trimws(email) == ""){
                                    message("The input email is NULL. Stopping execution.")
                                    stop("Function terminated because email is NULL.")
                                  }
                                  if(is.null(invite_sent_by) || trimws(invite_sent_by) == ""){
                                    message("The input invite_sent_by is NULL. Stopping execution.")
                                    stop("Function terminated because invite_sent_by is NULL.")
                                  }

                                  email_lowered <- tolower(email)

                                  if(is.null(groups)){
                                    parsed_groups <- ""
                                  } else {
                                    parsed_groups <- self$to_json(groups)
                                  }



                                  qu <- glue::glue("UPDATE {self$schema}.shiny_msal_accounts_pending_invites SET email=?email, username=?username, invite_sent_by=?invite_sent_by, invite_date_sent=NOW(), role=?role, groups=?parsed_groups, expire_date=NOW() + INTERVAL '7 days' WHERE invite_id = ?invite_id") %>% as.character()


                                  query <- DBI::sqlInterpolate(DBI::ANSI(),
                                                               qu,
                                                               email = email_lowered,
                                                               username = username,
                                                               invite_sent_by = invite_sent_by,
                                                               role = role,
                                                               parsed_groups = parsed_groups,
                                                               invite_id = invite_id)

                                  self$execute_query(query)

                                },


                                #' @description Remove an invitation for a user
                                #' @param inviteid id of invitation to be deleted
                                #' @param appname application name
                                remove_invite = function(inviteid, appname){

                                  if(is.null(inviteid) || trimws(inviteid) == ""){
                                    message("The input inviteid is NULL. Stopping execution.")
                                    stop("Function terminated because inviteid is NULL.")
                                  }
                                  if(is.null(appname) || trimws(appname) == ""){
                                    message("The input appname is NULL. Stopping execution.")
                                    stop("Function terminated because appname is NULL.")
                                  }

                                  qu <- glue::glue("delete from {self$schema}.shiny_msal_accounts_pending_invites where invite_id = ?invite_id and appname = ?appname") %>% as.character()

                                  query <- DBI::sqlInterpolate(DBI::ANSI(),
                                                               qu,
                                                               invite_id = inviteid,
                                                               appname = appname)

                                  self$execute_query(query)

                                },

                                ##### Extra logging #####

                                #' @description Add a logging entry for MSAL users
                                #' @param user_id id of user causing the action that is logged
                                #' @param appname application name
                                #' @param action_type Categorical type of action
                                #' @param logging_message Detailed logging message
                                make_msal_log = function(user_id, appname, action_type, logging_message){

                                  if(is.null(user_id) || trimws(user_id) == ""){
                                    message("The input user_id is NULL. Stopping execution.")
                                    stop("Function terminated because user_id is NULL.")
                                  }
                                  if(is.null(appname) || trimws(appname) == ""){
                                    message("The input appname is NULL. Stopping execution.")
                                    stop("Function terminated because appname is NULL.")
                                  }
                                  if(is.null(action_type) || trimws(action_type) == ""){
                                    message("The input action_type is NULL. Stopping execution.")
                                    stop("Function terminated because action_type is NULL.")
                                  }
                                  if(is.null(logging_message) || trimws(logging_message) == ""){
                                    message("The input logging_message is NULL. Stopping execution.")
                                    stop("Function terminated because logging_message is NULL.")
                                  }

                                  qu <- glue::glue("INSERT INTO {self$schema}.shiny_msal_logging (user_id, appname, action_type, logging_message) VALUES(?user_id, ?appname, ?action_type, ?logging_message)") %>% as.character()


                                  query <- DBI::sqlInterpolate(DBI::ANSI(),
                                                               qu,
                                                               user_id = user_id,
                                                               appname = appname,
                                                               action_type = action_type,
                                                               logging_message = logging_message)

                                  self$execute_query(query)

                                },


                                ##### Internal Use ######
                                #' @description Get list of available applications
                                #' @details !! Do not use in shiny applications (except shintousers_app) !!
                                get_applications = function(){

                                  sort(self$query(glue::glue("select appname from {self$schema}.applications"))$appname)

                                }


                              )

)

