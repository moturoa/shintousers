
#---- Script to initialize database schema

# Config
con <- shintobag::shinto_db_connection("admin_users", file = "c:/repos/conf/remko.yml")

library(DBI)
library(RPostgres)
library(dplyr)


# Make tables
dbExecute(con, "create table if not exists logins (
                  timestamp varchar,
                  userid varchar,
                  appname varchar)")


dbExecute(con, "create table if not exists roles (
                  userid varchar,
                  username varchar,
                  appname varchar,
                  role varchar,
                  comment varchar
          )")


# Grant access to apps
grant_app <- function(appusername){
  dbExecute(con, paste("grant select on roles to", appusername))
  dbExecute(con, paste("grant select, insert, update on logins to", appusername))
}

appusers <- c("wbm_eindhoven","wbm_groningen","wbm_zaanstad","risicoradar")

sapply(appusers, grant_app)

# Grant access to user shintoanalytics 
# (gaan we gebruiken voor de app die de roles kan zetten)
dbExecute(con, paste("grant select, insert, update on roles to shintoanalytics"))
dbExecute(con, paste("grant select, insert, update on logins to shintoanalytics"))










