#' Request Trend Data
#'
#' @param query Query in lucene's syntax
#' @param groupby The field (or fields, comma separated) to group and pivot the results. One or two (maximum) of: country, region, sentiment, source, lang, tags.
#' @param interval Interval to group the results. One of: hourly, daily, weekly, monthly, yearly
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
#' @param token Object with your token from palver::get_token
#'
#' @return A tibble from data required
#' @export
#'
request_trends <- function(
    query,
    groupby = NULL,
    interval = c('daily', 'weekly', 'monthly', 'yearly'),
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
    token){

  queryField <- match.arg(queryField)
  interval <- match.arg(interval)
  spam <- match.arg(spam)

  page <- 1
  perPage <- 1000

  fetch_trends <- function(query,
                           page = 1,
                           perPage = 1000,
                           groupby = NULL,
                           interval = c('daily', 'weekly', 'monthly', 'yearly'),
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
                           token){

    parameters <- list('query' = query,
                       'page' = page,
                       'perPage' = perPage,
                       'groupby' = groupby,
                       'interval' = interval,
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
                       'type_label' = type_label)


    httr::GET(url = 'https://api2.palver.com.br/rest/trends',
              config = httr::add_headers('Accept' = 'application/json',
                                         'Content-Type' = 'application/json',
                                         'Authorization' = stringr::str_c('Bearer ',token)),
              query = parameters)}

  response <- fetch_trends(query = query,
                           groupby = groupby,
                           country = country,
                           interval = interval,
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
                           token = token,
                           page = page)

  if(httr::status_code(response)==200){

    meta <- httr::content(response) %>%
      purrr::pluck('meta')  %>%
      tibble::enframe()

    totalPages <- meta %>%
      dplyr::filter(.data$name=='totalPages') %>%
      purrr::pluck('value') %>%
      unlist()

    data <- purrr::map(.x = page:totalPages,
                       .f = ~fetch_messages(query = query,
                                            groupby = groupby,
                                            country = country,
                                            interval = interval,
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
