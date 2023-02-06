
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

con <- shintodb::connect("shintousers")

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
                  attributes text,
                  groep text
          )")

dbExecute(con, "create table if not exists applications (
                  appname varchar,
                  roles json,
                  comment varchar
          )")

dbExecute(con, "create table if not exists timings (
                  appname varchar,
                  key varchar,
                  value double precision,
                  userid varchar,
                  timestamp timestamp without time zone default current_timestamp
          )")









