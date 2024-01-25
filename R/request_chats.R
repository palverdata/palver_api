#' Request Chat Data
#'
#' @param query Boolean query in lucene's syntax
#' @param id The group's unique identifier
#' @param name Exact group name
#' @param source Groups' origin platform
#' @param sortField Sort Field
#' @param sortOrder Sort Order
#' @param token Object with your token from palver::get_token
#'
#' @return A tibble from data required
#' @export
#'
#'

request_chats <- function(
    query = NULL,
    id = NULL,
    name = NULL,
    source = NULL,
    sortField = c('name', 'participants'),
    sortOrder = c('desc','asc'),
    token){

  sortField <- match.arg(sortField)
  sortOrder <- match.arg(sortOrder)

  page <- 1
  perPage <- 1000

  fetch_chats <- function(query,
                          id,
                          page = 1,
                          perPage = 1000,
                          name = NULL,
                          source = NULL,
                          sortField = c('name', 'participants'),
                          sortOrder = c('desc','asc'),
                          token){

    parameters <- list('query' = query,
                       'id' = id,
                       'name' = name,
                       'page' = page,
                       'perPage' = perPage,
                       'source' = source,
                       'sortField' = sortField,
                       'sortOrder' = sortOrder)


    httr::GET(url = 'https://api2.palver.com.br/rest/v2/chats',
              config = httr::add_headers('Accept' = 'application/json',
                                         'Content-Type' = 'application/json',
                                         'Authorization' = stringr::str_c('Bearer ',token)),
              query = parameters)}

  response <- fetch_chats(query = query,
                          id = id,
                          name = name,
                          source = source,
                          sortField = sortField,
                          sortOrder = sortOrder,
                          token = token)

  if(httr::status_code(response)==200){

    meta <- httr::content(response) %>%
      purrr::pluck('meta') %>%
      tibble::enframe()

    totalPages <- meta %>%
      dplyr::filter(.data$name=='totalPages') %>%
      purrr::pluck('value') %>%
      unlist()

    data <- purrr::map(.x = page:totalPages,
                       .f = ~fetch_chats(query = query,
                                         id = id,
                                         source = source,
                                         sortField = sortField,
                                         sortOrder = sortOrder,
                                         token = token,
                                         page = .x) %>%
                         httr::content(.) %>%
                         purrr::pluck('data') %>%
                         tibble::enframe() %>%
                         tidyr::unnest_wider('value', names_repair = 'minimal')
    ) %>%
      purrr::reduce(dplyr::bind_rows)

    return(data)

  }

  else stop(
    stringr::str_c(
      httr::content(response),
      ' [', httr::status_code(response),']'
    )
  )


}
