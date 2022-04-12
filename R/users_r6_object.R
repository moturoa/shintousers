
#' A User object for use in Shinto apps
#' @export
#' @importFrom dplyr tbl collect
shintoUser <- R6::R6Class(
  
  
  public = list(
    
    #' @description The DB connection to `shintousers`
    con = NULL,
    
    #' @description The postgres schema in the DB
    schema = NULL,
    
    #' @description The DB user name
    dbusername = NULL,
    
    #' @description The application (shiny) user name
    userid = NULL,
    
    #' @appname The (rsconnect) application name
    appname = NULL,
    
    
    initialize = function(dbusername = "shintousers", 
                          dbname = "shintousers",
                          schema = "shintousers",
                          userid = NULL, 
                          appname = NULL, 
                          con = NULL,
                          ...){
      
      
      self$schema <- schema
      
      if(is.null(con)){
        self$con <- users_db_connection(dbusername=dbusername, dbname=dbname,...)  
      } else {
        self$con <- con
      }
      
      
      self$userid <- userid
      
      if(is.null(appname)){
        appname <- get_appname_from_url()
        if(is.na(appname))stop("Provide appname or run from rsconnect!")
      }
      self$appname <- appname
      
    },
    
    #' @description Close the DB connection
    close = function(){
      
      dbDisconnect(self$con)
      
    },
    
    #' @description Do a `dbGetQuery` on the `shintousers` DB (accepts glue)
    query = function(txt, glue = TRUE){
      
      if(glue)txt <- glue::glue(txt)
      
      try(
        dbGetQuery(self$con, txt)
      )
      
    },
    
    #' @description Do a `dbExecute` on the `shintousers` DB (accepts glue)
    execute_query = function(txt, glue = TRUE){
      
      if(glue)txt <- glue::glue(txt)
      
      try(
        dbExecute(self$con, txt)
      )
      
    },
    
    #' @description `dbWriteTable(..., append = TRUE)`
    append_data = function(table, data){
      
      try(
        dbWriteTable(self$con,
                     Id(schema = self$schema, table = table),
                     value = data,
                     append = TRUE)
      )
      
    },
    
    #' @description Read a table (from the default schema)
    read_table = function(table, lazy = FALSE){
      
      out <- dplyr::tbl(self$con, in_schema(self$schema, table))
      
      if(!lazy){
        out <- dplyr::collect(out)
      }
      
      out
      
    },
    
    #' @description Get last login for this user for this application (reads `shintousers.logins`)
    get_last_login = function(userid = NULL, appname = NULL){
      
      if(is.null(userid))userid <- self$userid
      if(is.null(appname))appname <- self$appname
      
       out <- self$query(glue("SELECT timestamp FROM {self$schema}.logins WHERE 
                                  userid = '{userid}' and appname = '{appname}'"))
      
       if(nrow(out) == 0)return(NULL)
       as.POSIXct(out$timestamp, tz = "UTC")
    },
    
    #' @description Update the last login for this user / appname (in table `shintousers.logins`)
    set_last_login = function(now = as.character(Sys.time())){
      
      self$execute_query(glue("UPDATE {self$schema}.logins SET timestamp = '{now}' 
                                 WHERE userid = '{self$userid}' and appname = '{self$appname}'"))
      
    },
  
    #' @description Reads last login, and sets the current time as the new 'last login'. 
    #' @details If user has never logged in, writes a new line in `shintousers.logins`,
    #' otherwise updates the last login. 
    #' @returns Returns (invisibly) the last login information
    get_and_set_last_login = function(){
      
      user_log <- self$get_last_login()
      
      # User has not previously logged in
      if(is.null(user_log)){
        self$append_data(
          "logins",
          tibble(
              timestamp  = as.character(Sys.time()),
              userid = self$userid,
              appname = self$appname
          )
        )
        
        user_log <- self$get_last_login()
      }
      
      self$set_last_login()
      
    return(invisible(user_log))
    },
    
    #' @description Gets the role for the current user (admin or viewer, typically)
    get_role = function(userid = NULL, appname = NULL){
      
      if(is.null(userid))userid <- self$userid
      if(is.null(appname))appname <- self$appname
      
      out <- self$query(glue("select role from {self$schema}.roles where userid = '{userid}' and appname = '{appname}'"))
      
      if(nrow(out) == 0){
        return(NULL)
      }
        
      out$role
      
    },
    
    #' @description Sets the role for a user.
    #' @details !! Do not use in shiny applications (except shintousers_app) !!
    set_role = function(userid, appname, role){
      
      if(is.null(self$get_role(userid, appname))){
        
        self$append_data(
          "roles",
          tibble(
            userid = userid,
            appname = appname,
            role = role,
            comment = ""
          )
        )
        
      } else {
        
        self$execute_query(glue("UPDATE {self$schema}.roles SET role = '{role}' 
                                 WHERE userid = '{userid}' and appname = '{appname}'"))
        
      }
      
    },
    
    #' @description Get groups that the current user belongs to. See also `is_in_group`
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
    is_in_group = function(group){
      
      group %in% self$get_group()
      
    },
    
    #' @description Set the group for this user.
    #' @details !! Do not use in shiny applications (except shintousers_app) !!
    set_group = function(userid, appname, group){
      
      group <- jsonlite::toJSON(group)
      
      if(is.null(self$get_group(userid, appname))){

        self$append_data(
          "roles",
          tibble(
            userid = userid,
            appname = appname,
            groep = group
          )
        )

      } else {

        self$execute_query(glue("UPDATE {self$schema}.roles SET groep = '{group}' 
                                 WHERE userid = '{userid}' and appname = '{appname}'"))
        
      }
      
    },
    
    #' @description Remove a role for a user
    #' @details !! Do not use in shiny applications (except shintousers_app) !!
    remove_role = function(userid, appname){
    
      self$execute_query(glue("delete from {self$schema}.roles where userid = '{userid}' and appname = '{appname}'"))
      
    },
    
    #' @description Does the current user have a role? `$has_role("admin")` --> bool
    has_role = function(role){
      
      roles <- self$get_role()
      as.character(role) %in% as.character(roles)
      
    },
    
    #' @description Add an application to the list of applications
    #' @details !! Do not use in shiny applications (except shintousers_app) !!
    add_application = function(appname, roles, groups, comment = ""){
      
      data <- tibble(
        appname = appname,
        roles = jsonlite::toJSON(roles),
        groups = jsonlite::toJSON(groups),
        comment = comment
      )
      
      d_exists <- nrow(self$query(glue("select * from {self$schema}.applications where appname = '{appname}'"))) > 0
      
      if(!d_exists){
        self$append_data("applications", data)
      }
      
    },
    
    #' @description Set application available roles
    #' @details !! Do not use in shiny applications (except shintousers_app) !!
    set_application_roles = function(appname, roles){
      
      role_json <- jsonlite::toJSON(roles)
      self$execute_query(glue("update {self$schema}.applications set roles = '{role_json}' where appname = '{appname}'"))
      
    },
    
    #' @description Set groups available in an application
    #' @details !! Do not use in shiny applications (except shintousers_app) !!
    set_application_groups = function(appname, groups){
      
      group_json <- jsonlite::toJSON(groups)
      self$execute_query(glue("update {self$schema}.applications set groups = '{group_json}' where appname = '{appname}'"))
      
    },
    
    #' @description Get available roles for an application
    #' @details !! Do not use in shiny applications (except shintousers_app) !!
    get_application_roles = function(appname){
      
      out <- self$query(glue("select roles from {self$schema}.applications where appname = '{appname}'"))$roles
      
      if(all(is.na(out)) || length(out) == 0){
        return(NA)
      } else {
        return(jsonlite::fromJSON(out))
      }
      
    },
    
    #' @description Add an application to the list of applications
    #' @details !! Do not use in shiny applications (except shintousers_app) !!
    get_application_groups = function(appname){
      
      out <- self$query(glue("select groups from {self$schema}.applications where appname = '{appname}'"))$groups
      
      if(all(is.na(out)) || length(out) == 0 || out == ""){
        return(NA)
      } else {
        return(jsonlite::fromJSON(out))
      }
      
    },
    
    #' @description Get list of available applications
    #' @details !! Do not use in shiny applications (except shintousers_app) !!
    get_applications = function(){
      
      sort(self$query(glue("select appname from {self$schema}.applications"))$appname)
      
    },
    
    #' @description Remove an application 
    #' @details !! Do not use in shiny applications (except shintousers_app) !!
    remove_application = function(appname){
      
      self$execute_query(glue("delete from {self$schema}.applications where appname = '{appname}'"))
      
    }
    
    
    
  )
  
)

