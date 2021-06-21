

library(glue)
library(DBI)
library(dplyr)


devtools::load_all()

.user <- shintoUser$new("shintoanalytics", "remkoduursma", "testapp", config_file = "c:/repos/conf/config.yml")

.user$get_last_login()

.user$get_and_set_last_login()


.user$get_role()

.user$has_role("admin")
.user$has_role("admin2")


.user$remove_role("wesley", "riec_rro")
.user$set_role("wesley","riec_rro","viewer")
.user$get_role("wesley", "riec_rro")

.user$remove_role("wesley", "riec_rro")
.user$get_role("wesley", "riec_rro")

