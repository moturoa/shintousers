
#' A User object for use in Shinto apps
#' @description Make a database connection to 'shintousers' and access all kinds of 
#' methods for user management. In Shiny apps, use `get_and_set_last_login`, for example.
#' @importFrom dplyr tbl collect 
#' @importFrom DBI dbDisconnect dbConnect dbGetQuery dbExecute dbWriteTable Id
#' @importFrom glue glue
#' @importFrom jsonlite toJSON
#' @importFrom tibble tibble
#' @importFrom R6 R6Class
#' @importFrom dbplyr in_schema
#' @export
shintoUser <- R6::R6Class(classname = "ShintoUsers",
  
  
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
    #' @param dbusername Username in database 'shintousers' 
    #' @param dbname Name of database (usually 'shintousers')
    #' @param schema Database schema with tables 'roles', 'users', 'logins'
    #' @param userid rsconnect username, if not NULL it is stored and used for all methods (handy inside an app)
    #' @param appname rsconnect application name
    #' @param appversion Optional, application version string
    #' @param con Optional, existing database connection to shintousers (for recycling)
    #' @param ... Further arguments passed to [users_db_connection()]
    #' @return A 'shintousers' R6 object
    initialize = function(dbusername = "shintousers", 
                          dbname = "shintousers",
                          schema = "shintousers",
                          userid = NULL, 
                          appname = "", 
                          appversion = "",
                          con = NULL,
                          ...){
      
      
      self$schema <- schema
      
      if(is.null(con)){
        self$con <- users_db_connection(dbusername=dbusername, dbname=dbname,...)  
      } else {
        self$con <- con
      }
      
      
      self$userid <- userid
      
      self$appname <- appname
      self$appversion <- appversion
      
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
    
    
    #' @description Close the DB connection
    close = function(){
      
      DBI::dbDisconnect(self$con)
      
    },
    
    #' @description Do a `dbGetQuery` on the `shintousers` DB (accepts glue)
    #' @param txt Query string
    #' @param glue If true, attempts to 'glue'
    query = function(txt, glue = TRUE){
      
      if(glue)txt <- glue::glue(txt)
      
      try(
        DBI::dbGetQuery(self$con, txt)
      )
      
    },
    
    #' @description Do a `dbExecute` on the `shintousers` DB (accepts glue)
    #' @param txt Query string
    #' @param glue If true, attempts to 'glue'
    execute_query = function(txt, glue = TRUE){
      
      if(glue)txt <- glue::glue(txt)
      
      try(
        DBI::dbExecute(self$con, txt)
      )
      
    },
    
    #' @description `dbWriteTable(..., append = TRUE)`
    #' @param table table name (without schema)
    #' @param data dataframe
    append_data = function(table, data){
      
      try(
        DBI::dbWriteTable(self$con,
                     DBI::Id(schema = self$schema, table = table),
                     value = data,
                     append = TRUE)
      )
      
    },
    
    #' @description Read a table (from the default schema)
    #' @param table table name (without schema)
    #' @param lazy If true, returns a 'lazy table' (dbplyr)
    read_table = function(table, lazy = FALSE){
      
      out <- dplyr::tbl(self$con, dbplyr::in_schema(self$schema, table))
      
      if(!lazy){
        out <- dplyr::collect(out)
      }
      
      out
      
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
          tibble::tibble(
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
          tibble::tibble(
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
          tibble::tibble(
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
      
      roles <- self$get_role()
      isTRUE(as.character(role) %in% as.character(roles))
      
    },
    
    #' @description Add an application to the list of applications
    #' @details !! Do not use in shiny applications (except shintousers_app) !!
    #' @param roles The choices for user roles, typically c('admin','viewer')
    #' @param appname The rsconnect application name
    #' @param groups The choices for application groups, can be anything
    #' @param comment Any other text (unused as of 11/2022)
    add_application = function(appname, roles, groups, comment = ""){
      
      data <- tibble::tibble(
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
      
    }
    
    
    
  )
  
)

