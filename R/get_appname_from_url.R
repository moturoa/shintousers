#' Read path to appname on rsconnect
#' @export
get_appname_from_url <- function(session = getDefaultReactiveDomain()){
  
  if(is.null(session))return(NA_character_)
  
  urlpath <- session$clientData$url_pathname
  gsub("/","_",urlpath)
  
}
