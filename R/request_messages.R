#' Request Message Data
#'
#' @param query Query in lucene's syntax
#' @param country Country ISO code
#' @param region Brazilian region code, (only if country is BR)
#' @param startDate Start date in ISO format
#' @param endDate End date in ISO format
#' @param sentiment Sentiment of the message
#' @param source Source of the message
#' @param is_spam Include or exclude messages classified as spam
#' @param is_nsfw Include or exclude messages classified as NSFW
#' @param lang Language of the message
#' @param transcript_lang Language of the audio transcript
#' @param ocr_lang Language of the image OCR
#' @param tags Tags associated with the message's chat
#' @param type_label Type of content in message
#' @param sortField Sort Field
#' @param sortOrder Sort Order
#' @param token Object with your token from palver::get_token
#'
#' @return A tibble from data required
#' @export
#'

request_messages <- function(
    query,
    country = NULL,
    region = NULL,
    startDate,
    endDate,
    sentiment = NULL,
    source = NULL,
    is_spam = NULL,
    is_nsfw = NULL,
    lang = NULL,
    transcript_lang = NULL,
    ocr_lang = NULL,
    tags = NULL,
    type_label = NULL,
    sortField = c('datetime', 'forwarding_score'),
    sortOrder = c('desc','asc'),
    token){

  sortField <- match.arg(sortField)
  sortOrder <- match.arg(sortOrder)

  page <- 1
  perPage <- 1000

  fetch_messages <- function(query,
                             page = 1,
                             perPage = 1000,
                             country = NULL,
                             region = NULL,
                             startDate,
                             endDate,
                             sentiment = NULL,
                             source = NULL,
                             is_spam = NULL,
                             is_nsfw = NULL,
                             lang = NULL,
                             transcript_lang = NULL,
                             ocr_lang = NULL,
                             tags = NULL,
                             type_label = NULL,
                             sortField = c('datetime', 'forwarding_score'),
                             sortOrder = c('desc','asc'),
                             token){

    parameters <- list('query' = query,
                       'page' = page,
                       'perPage' = perPage,
                       'country' = country,
                       'region' = region,
                       'startDate' = startDate,
                       'endDate' = endDate,
                       'sentiment' = sentiment,
                       'source' = source,
                       'is_spam' = is_spam,
                       'is_nsfw' = is_nsfw,
                       'lang' = lang,
                       'transcript_lang' = transcript_lang,
                       'ocr_lang' = ocr_lang,
                       'tags' = tags,
                       'type_label' = type_label,
                       'sortField' = sortField,
                       'sortOrder' = sortOrder)


    httr::GET(url = 'https://api2.palver.com.br/rest/v2/messages',
              config = httr::add_headers('Accept' = 'application/json',
                                         'Authorization' = stringr::str_c('Bearer ',token)),
              query = parameters)}

  response <- fetch_messages(query = query,
                             page = page,
                             perPage = perPage,
                             country = country,
                             region = region,
                             startDate = startDate,
                             endDate = endDate,
                             sentiment = sentiment,
                             source = source,
                             is_spam = is_spam,
                             is_nsfw = is_nsfw,
                             lang = lang,
                             transcript_lang = transcript_lang,
                             ocr_lang = ocr_lang,
                             tags = tags,
                             type_label = type_label,
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
                       .f = ~fetch_messages(query = query,
                                            page = .x,
                                            perPage = perPage,
                                            country = country,
                                            region = region,
                                            startDate = startDate,
                                            endDate = endDate,
                                            sentiment = sentiment,
                                            source = source,
                                            is_spam = is_spam,
                                            is_nsfw = is_nsfw,
                                            lang = lang,
                                            transcript_lang = transcript_lang,
                                            ocr_lang = ocr_lang,
                                            tags = tags,
                                            type_label = type_label,
                                            sortField = sortField,
                                            sortOrder = sortOrder,
                                            token = token) %>%
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

