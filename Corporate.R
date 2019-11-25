library(httr)
library(askpass)

RESPONSE_REASON_OK = "OK"
RESPONSE_REASON_FAIL = "Proxy Authentication Reguired" 

# atomic; mutate by '<<-' in functions
atom_ResponseStatus = NULL;

getResponseStatus <-function() {
  url="http://www.google.com"
  http_status(GET(url))
}

isResponse <- function() {
  response = getResponseStatus()
  if (response$reason == RESPONSE_REASON_OK) {
    atom_ResponseStatus <<- response
    TRUE
  } else {
    atom_ResponseStatus <<- response
    FALSE
  }
}

getResponseReason <- function() {
  isResponse()
  return(atom_ResponseStatus$reason)
}

proxyValidation <- function() {
  if (getResponseReason() == RESPONSE_REASON_OK) {
    print("Connection Successful")
  } else {
    if (getResponseReason() == RESPONSE_REASON_FAIL) {
      # set proxy validation based on IEsettings/Windows (it's corporate right?)
      # ask for password
      set_config(
        use_proxy(url = curl::ie_get_proxy_for_url(),
                  # Windows property
                  username = Sys.getenv("USERNAME"),
                  pass = askpass("Connecion failed. Password for proxi.")))
    }
  }
  isResponse()
}

## DEV BLOCK
devblock <- function() {
  
  rm(list = ls())
  
  proxyValidation();
  atom_ResponseStatus
  
}

