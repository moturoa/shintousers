
#' Connect to the users database from an app
#' @param dbusername Database user name on devpostgres02 for this app ('risicoradar','wbm_eindhoven')
#' @export
users_db_connection <- function(dbusername, config_file = "conf/config.yml"){
  
    conf <- config::get(dbusername, file = config_file)
    
    DBI::dbConnect(RPostgres::Postgres(), 
                   dbname = "users", 
                   host = conf$dbhost, 
                   port = 5432, 
                   user = conf$dbuser, 
                   password = conf$dbpassword)
  
}





