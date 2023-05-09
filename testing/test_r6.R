
devtools::load_all()

#Sys.setenv(R_CONFIG_ACTIVE = "productionlocal")


cfg_file <- "c:/repos/wbm3.0/conf/config.yml"

.user <- shintoUser$new(userid = "apollo_dev",
                        appname = "demo_apollo",
                        appversion = "3.1", 
                        config_file = cfg_file)

.user$list_application_users()
.user$list_application_users(roles = "admin")
.user$list_application_users(groups = "coordinator")

.user$get_name("apollo_dev")

.user$get_user_attributes(c("apollo_dev","fdouvdf"), "demo_apollo")

.user$get_name(c("apollo_dev","fdouvdf"), "demo_apollo")


.user$disable_user("apollo_dev", "demo_apollo")
.user$get_user_attributes("apollo_dev", "demo_apollo")
.user$enable_user("apollo_dev", "demo_apollo")
.user$get_user_attributes("apollo_dev", "demo_apollo")


.user$disable_user("apollo_dev", "demo_apollo")
.user$list_application_users("demo_apollo")
.user$list_application_users("demo_apollo", active_only = FALSE)
.user$enable_user("apollo_dev", "demo_apollo")

.user$list_indexes()


.user$filter("logins", appname == "demo_apollo", appversion == "1.2.3")



