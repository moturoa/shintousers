

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


.user$remove_role("remkoduursma", "testapp")
.user$set_role("remkoduursma","testapp","viewer")
.user$get_role("remkoduursma", "testapp")

.user$add_application("testapp", roles = c("viewer","admin"))
.user$get_applications()
.user$get_application_roles("testapp")
.user$set_application_roles("testapp", roles = c("viewer","admin","public"))
.user$get_application_roles("testapp")
.user$set_application_roles("testapp", roles = c("viewer","admin"))
.user$remove_application("testapp")
