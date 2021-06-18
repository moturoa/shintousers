
#' A User object for use in Shinto apps
#' @export
#' @importFrom dplyr tbl collect
shintoUser <- R6::R6Class(
  
  
  public = list(
    
    con = NULL,
    dbusername = NULL,
    userid = NULL,
    appname = NULL,
    
    
    initialize = function(dbusername, userid, appname = NULL, ...){
      
      self$con <- users_db_connection(dbusername, ...)
      
      self$userid <- userid
      
      if(is.null(appname)){
        appname <- get_appname_from_url()
        if(is.na(appname))stop("Provide appname or run from rsconnect!")
      }
      self$appname <- appname
      
    },
    
    query = function(txt, glue = TRUE){
      
      if(glue)txt <- glue::glue(txt)
      
      try(
        dbGetQuery(self$con, txt)
      )
      
    },
    
    execute_query = function(txt, glue = TRUE){
      
      if(glue)txt <- glue::glue(txt)
      
      try(
        dbExecute(self$con, txt)
      )
      
    },
    
    append_data = function(table, data){
      
      try(
        dbWriteTable(self$con,
                     name = table,
                     value = data,
                     append = TRUE)
      )
      
    },
    
    read_table = function(table, lazy = FALSE){
      
      out <- dplyr::tbl(self$con, table)
      
      if(!lazy){
        out <- dplyr::collect(out)
      }
      
      out
      
    },
    
    get_last_login = function(){
      
       out <- self$query(glue("SELECT timestamp FROM logins WHERE 
                                  userid = '{self$userid}' and appname = '{self$appname}'"))
      
       if(nrow(out) == 0)return(NULL)
       as.POSIXct(out$timestamp, tz = "UTC")
    },
    
    set_last_login = function(now = as.character(Sys.time())){
      
      self$execute_query(glue("UPDATE logins SET timestamp = '{now}' 
                                 WHERE userid = '{self$userid}' and appname = '{self$appname}'"))
      
    },
    
    
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
      
    return(user_log)
    },
    
    
    get_role = function(userid = NULL, appname = NULL){
      
      if(is.null(userid))userid <- self$userid
      if(is.null(appname))appname <- self$appname
      
      out <- self$query(glue("select role from roles where userid = '{userid}' and appname = '{appname}'"))
      
      if(nrow(out) == 0){
        return(NULL)
      }
        
      out$role
      
    },
    
    set_role = function(userid, appname, role){
      
      
      if(is.null(self$get_role(userid))){
        
        self$append_data(
          "roles",
          tibble(
            userid = userid,
            username = "",
            appname = appname,
            role = role,
            comment = ""
          )
        )
        
      } else {
        
        self$execute_query(glue("UPDATE roles SET role = '{role}' 
                                 WHERE userid = '{userid}' and appname = '{appname}'"))
        
        
      }
      
    },
    
    has_role = function(role){
      
      roles <- self$get_role()
      as.character(role) %in% as.character(roles)
      
    }
    
  )
  
)

