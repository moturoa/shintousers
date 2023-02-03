
#' Connect to the users database from an app
#' @param dbusername Database user name on devpostgres02 for this app ('risicoradar','wbm_eindhoven')
#' @param dbname Database name
#' @param config_file Path to DB connection YAML
#' @importFrom config get
#' @importFrom RPostgres Postgres
#' @export
users_db_connection <- function(dbusername = "shintousers", 
                                dbname = "shintousers", 
                                config_file = "conf/config.yml"){
  
    conf <- config::get(dbusername, file = config_file)
    
    if(is.null(conf)){
      conf <- config::get(dbusername, config = "default", file = config_file)
    }
    
    port <- ifelse(is.null(conf$dbport),5432,conf$dbport)
    
    DBI::dbConnect(RPostgres::Postgres(), 
                   dbname = dbname, 
                   host = conf$dbhost, 
                   port = port, 
                   user = conf$dbuser, 
                   password = conf$dbpassword)
  
}





