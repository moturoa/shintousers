
#---- Script to initialize database schema

# Config

if(FALSE){
  shintodb::make_config()
  shintodb::add_config_entry("shintousers","shintousers", where = "development", encrypt = TRUE)
  shintodb::add_config_entry("shintousers","shintousers", where = "production", encrypt = TRUE)
}



library(DBI)
library(RPostgres)
library(dplyr)

.db <- shintodb::databaseClass$new(what = "shintousers", schema = "shintousers")


con <- shintodb::connect("shintousers")

# modify
dbExecute(con, "alter table shintousers.roles add column active boolean default true")








