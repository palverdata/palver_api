#' Get Token for Palver Data API
#'
#' @param email E-mail from your Palver account
#' @param password Password from your Palver account
#'
#' @return A token string to access Palver Data
#' @export


get_token <- function(email, password){

  response <- httr2::request('https://mercury-api.palver.com.br/api/auth') %>%
    httr2::req_method('POST') %>%
    httr2::req_headers('Accept' = 'application/json','Content-Type' = 'application/json') %>%
    httr2::req_body_json(list(email = email,password = password)) %>%
    httr2::req_perform()

  if (httr2::resp_status(response)==200){
    return(
      httr2::resp_body_json(response) %>%
        purrr::pluck('token')
    )}

  else {
    stop(httr2::resp_status_desc(response))
  }
}


