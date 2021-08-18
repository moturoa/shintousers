
library(shintousers)
library(glue)
library(DBI)

.user <- shintoUser$new(dbusername = "ede", 
                         userid = "frank.keeris", 
                         appname = "ede_linkit")

.user$get_role()

.user$has_role("admin")

.user$get_last_login()


.user$get_role(userid = "linkit_dev")
