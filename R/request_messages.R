#' Request Message Data
#'
#' @param query Query in lucene's syntax
#' @param country Country ISO code
#' @param region  Region code
#' @param startDate Start date in ISO format
#' @param endDate End date in ISO format
#' @param sentiment Sentiment of the message
#' @param source Source of the message
#' @param is_spam Include or exclude messages classified as spam
#' @param is_nsfw Include or exclude messages classified as NSFW
#' @param is_news_related Iclude or exclude messages that are related to news
#' @param is_potentially_misleading Include or exclude messages that contains news and are classified as potentially misleading
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
    source = c('whatsapp', 'telegram', 'reddit', 'tiktok', 'press', 'news', 'radio.medias', 'televsion', 'youtube', 'twitter'),
    is_spam = NULL,
    is_nsfw = NULL,
    is_news_related = NULL,
    is_potentially_misleading = NULL,
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
  source <- match.arg(source)

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
                             source = c('whatsapp', 'telegram', 'reddit', 'tiktok', 'press', 'news', 'radio.medias', 'televsion', 'youtube', 'twitter'),
                             is_spam = NULL,
                             is_nsfw = NULL,
                             is_news_related = NULL,
                             is_potentially_misleading = NULL,
                             lang = NULL,
                             transcript_lang = NULL,
                             ocr_lang = NULL,
                             tags = NULL,
                             type_label = NULL,
                             sortField = c('datetime', 'forwarding_score'),
                             sortOrder = c('desc','asc'),
                             token){

    url <- stringr::str_c('https://mercury-api.anax.com.br/api/', source, '/messages')

    parameters <- tibble::tibble('query' = query,
                       'page' = page,
                       'perPage' = perPage,
                       'country' = country,
                       'region' = region,
                       'startDate' = startDate,
                       'endDate' = endDate,
                       'sentiment' = sentiment,
                       'is_news_related' = is_news_related,
                       'is_potentially_misleading' = is_potentially_misleading,
                       'is_spam' = is_spam,
                       'is_nsfw' = is_nsfw,
                       'lang' = lang,
                       'transcript_lang' = transcript_lang,
                       'ocr_lang' = ocr_lang,
                       'tags' = tags,
                       'type_label' = type_label,
                       'sortField' = sortField,
                       'sortOrder' = sortOrder) %>%
      jsonify::to_json(unbox = T, by = 'column')

    httr2::request(url) |>
      httr2::req_method('POST') |>
      httr2::req_auth_bearer_token(token) |>
      httr2::req_headers('Accept' = 'application/json',
                         'Content-Type' = 'application/json') |>
      httr2::req_body_raw(body = parameters, type = 'application/json') |>
      httr2::req_perform()


    }

  response <- fetch_messages(query = query,
                             page = page,
                             perPage = perPage,
                             source = source,
                             country = NULL,
                             region = NULL,
                             startDate = startDate,
                             endDate = endDate,
                             sentiment = NULL,
                             is_news_related = NULL,
                             is_potentially_misleading = NULL,
                             is_spam = NULL,
                             is_nsfw = NULL,
                             lang = NULL,
                             transcript_lang = NULL,
                             ocr_lang = NULL,
                             tags = NULL,
                             type_label = NULL,
                             sortField = NULL,
                             sortOrder = NULL,
                             token = token)

  if(httr2::resp_status(response)==200){

    meta <- httr2::resp_body_json(response) %>%
      purrr::pluck('meta') %>%
      tibble::enframe()

    totalPages <- meta %>%
      dplyr::filter(.data$name=='totalPages') %>%
      purrr::pluck('value') %>%
      unlist()

    if(totalPages != 0) {

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
                                            is_news_related = is_news_related,
                                            is_potentially_misleading = is_potentially_misleading,
                                            lang = lang,
                                            transcript_lang = transcript_lang,
                                            ocr_lang = ocr_lang,
                                            tags = tags,
                                            type_label = type_label,
                                            sortField = sortField,
                                            sortOrder = sortOrder,
                                            token = token) %>%
                         httr2::resp_body_json(.) %>%
                         purrr::pluck('data') %>%
                         tibble::enframe() %>%
                         tidyr::unnest_wider('value', names_repair = 'minimal')
    ) %>%
      purrr::reduce(dplyr::bind_rows)

    return(data)
    }

    else stop('This request has null data')}

  else stop(
    stringr::str_c(
      httr2::resp_status(response),
      ' [', httr2::resp_status_desc(response),']'
    )
  )


}

