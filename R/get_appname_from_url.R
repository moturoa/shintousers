#' Read path to appname on rsconnect
get_appname_from_url <- function(session = getDefaultReactiveDomain()){
  

  if(is.null(session))return(NA)
  
  urlpath <- session$clientData$url_pathname
  gsub("/","",urlpath)
  
}
