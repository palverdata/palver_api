#' Request Chat Data
#'
#' @param query Boolean query in lucene's syntax
#' @param id The group's unique identifier
#' @param name Exact group name
#' @param source Groups' origin platform
#' @param sortField Sort Field
#' @param sortOrder Sort Order
#' @param token Object with your token from function get_token
#'
#' @return A tibble from data required
#' @export
#'
#'

request_chats <- function(
    query = NULL,
    id = NULL,
    name = NULL,
    source = c('whatsapp', 'telegram', 'reddit', 'tiktok', 'press', 'news', 'radio.medias', 'television', 'youtube', 'twitter'),
    sortField = c('name', 'participants'),
    sortOrder = c('desc','asc'),
    token){

  sortField <- match.arg(sortField)
  sortOrder <- match.arg(sortOrder)
  source <- match.arg(source)

  page <- 1
  perPage <- 1000

  fetch_chats <- function(query = NULL,
                          id = NULL,
                          page = 1,
                          perPage = 1000,
                          name = NULL,
                          source =  c('whatsapp', 'telegram', 'reddit', 'tiktok', 'press', 'news', 'radio.medias', 'television', 'youtube', 'twitter'),
                          sortField = c('name', 'participants'),
                          sortOrder = c('desc','asc'),
                          token){

    url <- stringr::str_c('https://mercury-api.anax.com.br/api/', source, '/chats')

    parameters <- list('query' = query,
                       'id'= id,
                       'name' = name,
                       'page' = page,
                       'perPage' = perPage,
                       'sortField' = sortField,
                       'sortOrder' = sortOrder) %>%
      purrr::discard(is.null)

    httr2::request(url) %>%
      httr2::req_method('POST') %>%
      httr2::req_headers('accept' = 'application/json',
                         'Content-Type' = 'application/json') %>%
      httr2::req_auth_bearer_token(token) %>%
      httr2::req_body_json(data = parameters) %>%
      httr2::req_perform()
  }

  response <- fetch_chats(query = query,
                          id = id,
                          name = name,
                          source = source,
                          sortField = sortField,
                          sortOrder = sortOrder,
                          token = token)

  if(httr2::resp_status(response)==200){

    meta <- httr2::resp_body_json(response) %>%
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
                         httr2::resp_body_json(.) %>%
                         purrr::pluck('data') %>%
                         tibble::enframe() %>%
                         tidyr::unnest_wider('value', names_repair = 'minimal')
    ) %>%
      purrr::reduce(dplyr::bind_rows)

    return(data)

  }

  else stop(httr2::resp_status_desc(response))
}
