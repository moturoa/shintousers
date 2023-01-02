
library(shintousers)
library(glue)
library(DBI)

.user <- shintoUser$new(dbusername = "shintousers", 
                        dbname = "shintousers",
                        userid = "wbm_dev", 
                        appname = "groningen_wbm",
                        appversion = "3.6.0")


.user$get_role()

.user$has_role("admin")

.user$get_last_login()


.user$get_role(userid = "linkit_dev")
