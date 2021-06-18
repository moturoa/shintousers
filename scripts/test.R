

library(glue)
library(DBI)
library(dplyr)

source("scripts/users_db_connection.R")
source("scripts/users_r6_object.R")

.user <- shintoUser$new("shintoanalytics", "remkoduursma", "testapp", config_file = "c:/repos/conf/config.yml")

.user$get_last_login()

.user$get_and_set_last_login()


.user$get_role()

.user$has_role("admin")
.user$has_role("admin2")


.user$set_role("wesley","riec_rro","viewer")
.user$get_role("wesley", "riec_rro")
