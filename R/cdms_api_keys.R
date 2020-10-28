#------------------------------------------------------------------------------
# Helper function which should not be included in GitHub repository.  The
# function sets the CDMS API username and security key for accessing CDMS
# and publicly available information. Restricted access data is accessed
# only through individual user logins.
#------------------------------------------------------------------------------

cdmsKeys <- function(){

  # Set Tribal Specific API Variables ----
  cdms_host <- 'https://npt-cdms.nezperce.org/'
  username <- 'api_user'
  #api_key <- '153054453130053281239582410943958241094537726538860542262540750542640375910349488180619663'
  api_key <- 'api_user'
  return(list = c(cdms_host, username, api_key))
}
