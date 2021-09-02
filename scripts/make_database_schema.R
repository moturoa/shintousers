
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
                  appname varchar,
                  role varchar,
                  comment varchar,
                  attributes text
          )")

dbExecute(con, "create table if not exists applications (
                  appname varchar,
                  roles json,
                  comment varchar
          )")

# # test
# dbWriteTable(con, "applications", tibble(appname = "test", 
#                                          roles = jsonlite::toJSON(c("admin","viewer")),
#                                          comment = ""
#                                          ),
#              append = TRUE)
# dbExecute(con, "truncate applications")

# Grant access to apps
grant_app <- function(appusername){
  dbExecute(con, paste("grant select on roles to", appusername))
  dbExecute(con, paste("grant select, insert, update on logins to", appusername))
}

appusers <- c("wbm_eindhoven","wbm_groningen","wbm_zaanstad","risicoradar","ede_linkit")

sapply(appusers, grant_app)

# Grant access to user shintoanalytics 
# (gaan we gebruiken voor de app die de roles kan zetten)
dbExecute(con, paste("grant select, insert, update, delete on roles to shintoanalytics"))
dbExecute(con, paste("grant select, insert, update on logins to shintoanalytics"))










