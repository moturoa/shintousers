
#' Connect to the users database from an app
#' @param dbusername Database user name on devpostgres02 for this app ('risicoradar','wbm_eindhoven')
#' @export
users_db_connection <- function(dbusername = "shintousers", config_file = "conf/config.yml"){
  
    conf <- config::get(dbusername, file = config_file)
    
    # cfg. allow_default_fallback in shintobag::shinto_db_connection
    if(is.null(conf)){
      conf <- config::get(dbusername, config = "default", file = config_file)
    }
    
    port <- ifelse(is.null(conf$dbport),5432,conf$dbport)
    
    DBI::dbConnect(RPostgres::Postgres(), 
                   dbname = "users", 
                   host = conf$dbhost, 
                   port = port, 
                   user = conf$dbuser, 
                   password = conf$dbpassword)
  
}





