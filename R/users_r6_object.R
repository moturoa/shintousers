
#' A User object for use in Shinto apps
#' @description Make a database connection to 'shintousers' and access all kinds of 
#' methods for user management. In Shiny apps, use `get_and_set_last_login`, for example.
#' @importFrom dplyr collect select filter arrange
#' @importFrom glue glue
#' @importFrom jsonlite toJSON
#' @importFrom shiny getDefaultReactiveDomain isRunning
#' @importFrom R6 R6Class
#' @importFrom shintodb databaseClass
#' @export
shintoUser <- R6::R6Class(classname = "ShintoUsers", 
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
    
    #' @field appname The (rsconnect) application name
    appname = NULL,
    
    #' @field appversion Version of the app (optionally read from VERSION)
    appversion = NULL,
    
    #' @description Make new shintousers object
    #' @param schema Database schema with tables 'roles', 'users', 'logins'
    #' @param userid rsconnect username, if not NULL it is stored and used for all methods (handy inside an app)
    #' @param appname rsconnect application name
    #' @param appversion Optional, application version string
    #' @param pool Passed to [shintodb::connect()]
    #' @param dbusername ignored ("shintousers")
    #' @param dbname ignored ("shintousers")
    #' @param con Optional, existing database connection to shintousers (for recycling)
    #' @param ... Further arguments passed to [shintodb::connect()]
    #' @return A 'shintousers' R6 object
    initialize = function(userid = NULL, 
                          appname = "", 
                          appversion = "",
                          default_user = "unknown",
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
      
      if(is.null(userid)){
        userid <- self$get_shiny_user(default = default_user)
      }
      
      self$userid <- userid
      self$appname <- appname
      self$appversion <- appversion
      
      self$ad_groups <- ad_groups
      self$ad_authentication <- ad_authentication
      self$admin_group_pattern <- admin_group_pattern
      
    },
    
    #' @description Get current shiny user  
    #' @param default Default user (returned if user is NULL)
    #' @param session Shiny session object (no need to set)
    get_shiny_user = function (default, session = shiny::getDefaultReactiveDomain()){
      
      if(!shiny::isRunning() || is.null(session$user)){
        default
      } else {
        session$user
      }
      
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
    
    #' @description Read attribute 'naam' for a user (default = current user)
    #' @param userid Vector of user ID's
    #' @param appname Application name (can be NULL, appname on init is then used)
    get_name = function(userid = NULL, appname = NULL){
      
      if(is.null(appname))appname <- self$appname
      if(is.null(userid))userid <- self$userid
      
      out <- self$get_user_attributes(userid, appname)
      
      out$naam <- sapply(out$attributes, function(x){
        if(is.na(x)){
          NA_character_
        } else {
          j <- jsonlite::fromJSON(x)$naam
          if(j == "")j <- NA_character_
          j
        }
      }, USE.NAMES = FALSE)
      
      out$naam[is.na(out$naam)] <- out$userid[is.na(out$naam)]
      
      ii <- match(userid, out$userid)
      out_names <- out$naam[ii]
      out_names[is.na(out_names)] <- userid[is.na(out_names)]
      out_names
    },
    
    #' @description Get last login for this user for this application (reads `shintousers.logins`)
    #' @param userid rsconnect username
    #' @param appname rsconnect application name
    get_last_login = function(userid = NULL, appname = NULL){
      
      if(is.null(userid))userid <- self$userid
      if(is.null(appname))appname <- self$appname
      
       out <- self$query(glue::glue("SELECT timestamp, appversion FROM {self$schema}.logins WHERE 
                                  userid = '{userid}' and appname = '{appname}'"))
      
       if(nrow(out) == 0)return(NULL)
       setNames(as.POSIXct(out$timestamp, tz = "UTC"), out$appversion)
    },
    
    #' @description Update the last login for this user / appname (in table `shintousers.logins`)
    #' @param now Optional, time string (defaults to sys time)
    #' @param userid rsconnect username (can be NULL, uses userid on init)
    #' @param appname rsconnect application name
    #' @param appversion application version string
    set_last_login = function(now = as.character(Sys.time()),
                              userid = NULL, appname = NULL, appversion = NULL){
      
      if(is.null(userid))userid <- self$userid
      if(is.null(appname))appname <- self$appname
      if(is.null(appversion))appversion <- self$appversion
      
      query <- glue::glue("UPDATE {self$schema}.logins SET timestamp = '{now}', appversion = '{appversion}' ",
                          "WHERE userid = '{userid}' and appname = '{appname}'")
      
      self$execute_query(query)
      
    },
  
    #' @description Reads last login, and sets the current time as the new 'last login'. 
    #' @details If user has never logged in, writes a new line in `shintousers.logins`,
    #' otherwise updates the last login. 
    #' @returns Returns (invisibly) the last login information
    #' @param userid rsconnect username (can be NULL, uses userid on init)
    #' @param appname rsconnect application name
    #' @param appversion application version string
    get_and_set_last_login = function(userid = NULL, appname = NULL, appversion = NULL){
      
      user_log <- self$get_last_login()
      
      if(is.null(userid))userid <- self$userid
      if(is.null(appname))appname <- self$appname
      if(is.null(appversion))appversion <- self$appversion
      
      # User has not previously logged in
      if(is.null(user_log)){
        self$append_data(
          "logins",
          data.frame(
              timestamp  = as.character(Sys.time()),
              userid = userid,
              appname = appname,
              appversion = appversion
          )
        )
        
        user_log <- self$get_last_login()
      }
      
      self$set_last_login()
      
    return(invisible(user_log))
    },
    
    #' @description Does an app have a user configured?
    #' @param userid rsconnect username (can be NULL, uses userid on init)
    #' @param appname rsconnect application name
    app_has_user = function(userid, appname){
      
      out <- self$query(glue::glue("select * from {self$schema}.roles where userid = '{userid}' and appname = '{appname}'"))
      nrow(out) > 0
      
    },
    
    #' @description List users for an app
    #' @param appname rsconnect application name
    list_application_users = function(appname = NULL,
                                      roles = NULL, 
                                      groups = NULL, 
                                      active_only = TRUE,
                                      ignore_groups = NULL,
                                      by_group = FALSE,
                                      add_last_login = FALSE){
      
      
      if(is.null(appname))appname <- self$appname
      
      data <- self$read_table("roles", lazy = TRUE) |>
        filter(appname == !!appname) |>
        dplyr::collect()
      
      data[["username"]] <- self$get_name(data[["userid"]])
      
      data <- dplyr::arrange(data, username) |>
        dplyr::select(userid, username, groep, role, active)
      
      if(active_only){
        data <- dplyr::filter(data, active)
      }
      
      if(!is.null(roles)){
        data <- dplyr::filter(data, role %in% !!roles)
      }
      
      if(!is.null(groups)){
        data <- dplyr::filter(data, 
                       grepl(paste(groups,collapse="|"), groep))  
      }
      
      if(!is.null(ignore_groups)){
        
        data <- dplyr::filter(data, 
                       !grepl(paste(ignore_groups,collapse="|"), groep))  
        
      }
      
      if(add_last_login){
        
        last_logins <- self$read_table("logins", lazy = TRUE) %>%
          dplyr::filter(appname == !!appname) %>%
          dplyr::select(userid, last_login = timestamp) %>%
          dplyr::collect() %>%
          dplyr::distinct(userid, .keep_all = TRUE)
        
        data <- dplyr::left_join(data, last_logins, by = "userid")
        
      }
      
      
      if(by_group){
        
        all_groups <- unique(do.call(c, lapply(data$groep, self$from_json)))
        
        data <- lapply(all_groups, function(g){
          dplyr::filter(data, grepl(g, groep))
        })
        
        names(data) <- all_groups        
        
      }

      return(data)
      
    },
    
    #' @description Set attributes for a user
    #' @param userid rsconnect username (can be NULL, uses userid on init)
    #' @param appname rsconnect application name
    #' @param attributes a list
    set_user_attributes = function(userid, appname, attributes = list(), active = TRUE){
      
      atr_json <- self$to_json(attributes)
      
      query <- glue::glue("UPDATE {self$schema}.roles SET attributes = '{atr_json}', active = {tolower(active)} ",
                          "WHERE userid = '{userid}' and appname = '{appname}'")
      
      self$execute_query(query)
    },
    
    #' @description Get attributes for a user
    #' @param userid rsconnect username (can be NULL, uses userid on init)
    #' @param appname rsconnect application name
    get_user_attributes = function(userid, appname){
      
      out <- self$read_table("roles", lazy = TRUE) |>
        filter(userid %in% !!userid, appname == !!appname) |>
        select(userid, attributes, active) |>
        collect()
      
      if(nrow(out) == 0){
        return(NULL)
      }
      
      out
    },
    
    #' @description Gets the role for the current user (admin or viewer, typically)
    #' @param userid rsconnect username (can be NULL, uses userid on init)
    #' @param appname rsconnect application name
    get_role = function(userid = NULL, appname = NULL){
      
      if(is.null(userid))userid <- self$userid
      if(is.null(appname))appname <- self$appname
      
      out <- self$query(glue::glue("select role from {self$schema}.roles where userid = '{userid}' and appname = '{appname}'"))
      
      if(nrow(out) == 0){
        return(NULL)
      }
        
      out$role
      
    },
    
    #' @description Sets the role for a user.
    #' @details !! Do not use in shiny applications (except shintousers_app) !!
    #' @param userid rsconnect username (can be NULL, uses userid on init)
    #' @param appname rsconnect application name
    #' @param role The user role, typically 'admin', 'viewer'
    set_role = function(userid, appname, role){
      
      if(!self$app_has_user(userid, appname)){
        
        self$append_data(
          "roles",
          data.frame(
            userid = userid,
            appname = appname,
            role = role,
            comment = ""
          )
        )
        
      } else {
        
        self$execute_query(glue::glue("UPDATE {self$schema}.roles SET role = '{role}' 
                                 WHERE userid = '{userid}' and appname = '{appname}'"))
        
      }
      
    },
    
    #' @description Make a user active or inactive (sets 'active' field in 'roles' table)
    #' @param userid User ID
    #' @param appname Application name
    #' @param what Either 'active' or 'inactive'
    set_user_active_inactive = function(userid, appname, what = c("active","inactive")){
      
      what <- match.arg(what)
      val <- what == "active"
      
      if(!self$app_has_user(userid, appname)){
        
        message("Tried to disable a non-existent user ($enable_disable_user in shintousers)")
        return(NULL)
        
      } else {
        
        self$execute_query(glue::glue("UPDATE {self$schema}.roles SET active = {tolower(val)} 
                                 WHERE userid = '{userid}' and appname = '{appname}'"))
        
      }
      
    },
    
    #' @description Make the user inactive for an app
    #' @param userid User ID
    #' @param appname Application name
    disable_user = function(userid, appname){
      
      self$set_user_active_inactive(userid, appname, "inactive")
      
    },
    
    #' @description Make the user active for an app
    #' @param userid User ID
    #' @param appname Application name
    enable_user = function(userid, appname){
      
      self$set_user_active_inactive(userid, appname, "active")
      
    },
    
    #' @description Get groups that the current user belongs to. See also `is_in_group`
    #' @param userid rsconnect username (can be NULL, uses userid on init)
    #' @param appname rsconnect application name
    get_group = function(userid = NULL, appname = NULL){
      
      if(is.null(userid))userid <- self$userid
      if(is.null(appname))appname <- self$appname
      
      out <- self$query(glue::glue("select groep from {self$schema}.roles where userid = '{userid}' and appname = '{appname}'"))
      
      if(nrow(out) == 0 || is.na(out$groep[1]) || out$groep == ""){
        return(NULL)
      }
      
      jsonlite::fromJSON(out$groep)
      
    },
    
    #' @description Is the user in this group? `$is_in_group("superuser")` -> bool
    #' @param group Group name
    is_in_group = function(group){
      
      isTRUE(group %in% self$get_group())
      
    },
    
    #' @description Set the group for this user.
    #' @details !! Do not use in shiny applications (except shintousers_app) !!
    #' @param group Group name
    #' @param userid rsconnect username (can be NULL, uses userid on init)
    #' @param appname rsconnect application name
    set_group = function(userid, appname, group){
      
      if(is.null(group))return(NULL)
      
      group <- self$to_json(group)
      
      if(!self$app_has_user(userid, appname)){

        self$append_data(
          "roles",
          data.frame(
            userid = userid,
            appname = appname,
            groep = group
          )
        )

      } else {

        self$execute_query(glue::glue("UPDATE {self$schema}.roles SET groep = '{group}' 
                                 WHERE userid = '{userid}' and appname = '{appname}'"))
        
      }
      
    },
    
    #' @description Remove a role for a user
    #' @details !! Do not use in shiny applications (except shintousers_app) !!
    #' @param userid rsconnect username (can be NULL, uses userid on init)
    #' @param appname rsconnect application name
    remove_role = function(userid, appname){
    
      self$execute_query(glue::glue("delete from {self$schema}.roles where userid = '{userid}' and appname = '{appname}'"))
      
    },
    
    #' @description Does the current user have a role? `$has_role("admin")` --> bool
    #' @param role The user role, typically 'admin', 'viewer'
    has_role = function(role){
      
      if(role == "admin" && self$ad_authentication){
        self$is_admin()
      } else {
        roles <- self$get_role()
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
    
    #' @description Add an application to the list of applications
    #' @details !! Do not use in shiny applications (except shintousers_app) !!
    #' @param roles The choices for user roles, typically c('admin','viewer')
    #' @param appname The rsconnect application name
    #' @param groups The choices for application groups, can be anything
    #' @param comment Any other text (unused as of 11/2022)
    add_application = function(appname, roles, groups = list(), comment = ""){
      
      data <- data.frame(
        appname = appname,
        roles = jsonlite::toJSON(roles),
        groups = jsonlite::toJSON(groups),
        comment = comment
      )
      
      out <- self$query(glue::glue("select * from {self$schema}.applications where appname = '{appname}'"))
      d_exists <- nrow(out) > 0
      
      if(!d_exists){
        self$append_data("applications", data)
      }
      
    },
    
    #' @description Set application available roles
    #' @details !! Do not use in shiny applications (except shintousers_app) !!
    #' @param roles The choices for user roles, typically c('admin','viewer')
    #' @param appname The rsconnect application name
    set_application_roles = function(appname, roles){
      
      role_json <- self$to_json(roles)
      self$execute_query(glue::glue("update {self$schema}.applications set roles = '{role_json}' where appname = '{appname}'"))
      
    },
    
    #' @description Set groups available in an application
    #' @details !! Do not use in shiny applications (except shintousers_app) !!
    #' @param groups The choices for application groups, can be anything
    #' @param appname The rsconnect application name
    set_application_groups = function(appname, groups){
      
      group_json <- self$to_json(groups)
      self$execute_query(glue::glue("update {self$schema}.applications set groups = '{group_json}' where appname = '{appname}'"))
      
    },
    
    #' @description Get available roles for an application
    #' @details !! Do not use in shiny applications (except shintousers_app) !!
    #' @param appname The rsconnect application name
    get_application_roles = function(appname){
      
      out <- self$query(glue::glue("select roles from {self$schema}.applications where appname = '{appname}'"))$roles
      
      if(all(is.na(out)) || length(out) == 0){
        return(NA)
      } else {
        return(self$from_json(out))
      }
      
    },
    
    #' @description Add an application to the list of applications
    #' @details !! Do not use in shiny applications (except shintousers_app) !!
    #' @param appname The rsconnect application name
    get_application_groups = function(appname){
      
      out <- self$query(glue::glue("select groups from {self$schema}.applications where appname = '{appname}'"))$groups
      
      if(all(is.na(out)) || length(out) == 0 || out == ""){
        return(NA)
      } else {
        return(self$from_json(out))
      }
      
    },
    
    #' @description Get list of available applications
    #' @details !! Do not use in shiny applications (except shintousers_app) !!
    get_applications = function(){
      
      sort(self$query(glue::glue("select appname from {self$schema}.applications"))$appname)
      
    },
    
    #' @description Remove an application 
    #' @details !! Do not use in shiny applications (except shintousers_app) !!
    #' @param appname The rsconnect application name
    remove_application = function(appname){
      
      self$execute_query(glue::glue("delete from {self$schema}.applications where appname = '{appname}'"))
      
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
      
      self$append_data(
        "timings",
        data.frame(
          appname = self$appname,
          key = key,
          value = v_n,
          userid  = self$userid
        )
      )
        
      
    }
    
    
  )
  
)

