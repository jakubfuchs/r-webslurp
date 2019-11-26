# for source reference handling
CorporateSourced <- function(){
  library(askpass)
  library(httr)
  "Corporate is sourced"
} 
CorporateSourced()

RESPONSE_REASON_OK = "OK"
RESPONSE_REASON_FAIL = "Proxy Authentication Required" 
RESPONSE_REASON_ERR = "Error"

# atomic; mutate by '<<-' in functions
atom_ResponseStatus = NULL;

getResponseStatus <-function() {
  # TODO better tryCatch...
  url="http://www.google.com"
  response = tryCatch(GET(URL),error = function(e) NULL, finally = NULL)
  if (!is.null(response)) {
    http_status(response)
  } else {
    response = c();
    response$reason = "Error"
    response
  }
}

isResponse <- function() {
  status = getResponseStatus()
  
  if (status$reason == RESPONSE_REASON_OK) {
    atom_ResponseStatus <<- status
    TRUE
  } else {
    atom_ResponseStatus <<- status
    FALSE
  }
}

getResponseReason <- function() {
  isResponse()
  return(atom_ResponseStatus$reason)
}

setProxy <- function(prompt = "Pass for Proxy:") {
  # set proxy validation based on IEsettings/Windows (it's corporate right?)
  # ask for password
  # TODO improve - flow control recursion, info if proxy found...
  pass = askpass(prompt)
  set_config(
    use_proxy(url = curl::ie_get_proxy_for_url(),
              # Windows env property
              username = Sys.getenv("USERNAME"),
              password = pass))
}

proxyValidation <- function() {
  reason = getResponseReason()
  if (reason == RESPONSE_REASON_OK) {
    print("Connection Successful")
    return()
  } else if (reason == RESPONSE_REASON_FAIL) {
    setProxy("Connecion failed on proxi. Password for user authorization:")
  } else if (reason == RESPONSE_REASON_ERR) {
    setProxy("Connection Error. Set proxi password and try again:")
  }
}

## DEV BLOCK
comment <- function() {
  # dev block
  
  rm(list = ls())
  
  proxyValidation();
  atom_ResponseStatus
}

