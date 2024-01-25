#' Get Token for Palver Data API
#'
#' @param email E-mail from your Palver account
#' @param password Password from your Palver account
#'
#' @return A token string to access Palver Data
#' @export


get_token <- function(email,password){

  response <- httr::POST(
    url = 'https://api2.palver.com.br/rest/v2/login',
    query = list('email' = email,
                 'password' = password),
    config = httr::add_headers('Accept' = 'application/json',
                               'Content-Type' = 'application/json'),
    encode = 'form'
  )

  if (httr::status_code(response)==200){
    return(
      httr::content(response) %>%
        purrr::pluck('token')
    )}

  else stop(
    stringr::str_c(
      httr::content(response),
      ' [', httr::status_code(response),']'
    )
  )
}


