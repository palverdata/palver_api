#' Request Time Series Data
#'
#' @param query Query in lucene's syntax
#' @param groupby The field (or fields, comma separated) to group and pivot the results. One or two (maximum) of: country, region, sentiment, source, lang, tags.
#' @param time_format Interval to group the results. One of: hourly, daily, weekly, monthly, yearly
#' @param country Country from area code
#' @param region Brazilian region code, (only if country is BR)
#' @param startDate Start date in ISO format
#' @param endDate End date in ISO format
#' @param sentiment Sentiment of the message
#' @param source Source of the message
#' @param is_news_related Include or exclude messages that are related to news
#' @param is_potentially_misleading Include or exclude messages that contains news and are classified as potentially misleading
#' @param spam Include or exclude messages classified as spam
#' @param nsfw Include or exclude messages classified as NSFW
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
    source = c('whatsapp', 'telegram', 'reddit', 'tiktok', 'press', 'news', 'radio.medias', 'television', 'youtube', 'twitter'),
    time_format = c('hourly','daily', 'weekly', 'monthly', 'yearly'),
    country = NULL,
    region = NULL,
    startDate,
    endDate,
    sentiment = NULL,
    is_news_related = NULL,
    is_potentially_misleading = NULL,
    spam = NULL,
    nsfw = NULL,
    lang = NULL,
    transcript_lang = NULL,
    ocr_lang = NULL,
    tags = NULL,
    type_label = NULL,
    token){

  time_format <- match.arg(time_format)
  source <- match.arg(source)

  fetch_trends <- function(query,
                           country = NULL,
                           region = NULL,
                           startDate,
                           endDate,
                           sentiment = NULL,
                           is_news_related = NULL,
                           is_potentially_misleading = NULL,
                           spam = NULL,
                           nsfw = NULL,
                           lang = NULL,
                           transcript_lang = NULL,
                           ocr_lang = NULL,
                           tags = NULL,
                           type_label = NULL,
                           groupby = NULL,
                           time_format = c('hourly','daily', 'weekly', 'monthly', 'yearly'),
                           source = c('whatsapp', 'telegram', 'reddit', 'tiktok', 'press', 'news', 'radio.medias', 'television', 'youtube', 'twitter'),
                           token){

    parameters <- list(query = query,
                       country = country,
                       region = region,
                       startDate = startDate,
                       endDate = endDate,
                       sentiment = sentiment,
                       is_news_related = is_news_related,
                       is_potentially_misleading = is_potentially_misleading,
                       spam = spam,
                       nsfw = nsfw,
                       lang = lang,
                       transcript_lang = transcript_lang,
                       ocr_lang = ocr_lang,
                       tags = tags,
                       type_label = type_label,
                       time_format = time_format,
                       groupby = groupby) %>%
      purrr::discard(is.null)

    url <- stringr::str_c('https://mercury-api.palver.com.br/api/', source, '/timeseries')

    httr2::request(url) |>
      httr2::req_method('POST') |>
      httr2::req_auth_bearer_token(token) |>
      httr2::req_body_json(data = parameters) |>
      httr2::req_perform()

  }

  response <- fetch_trends(query = query,
                           country = country,
                           region = region,
                           startDate = startDate,
                           endDate = endDate,
                           sentiment = sentiment,
                           is_news_related = is_news_related,
                           is_potentially_misleading = is_potentially_misleading,
                           spam = spam,
                           nsfw = nsfw,
                           lang = lang,
                           transcript_lang = transcript_lang,
                           ocr_lang = ocr_lang,
                           tags = tags,
                           type_label = type_label,
                           groupby = groupby,
                           time_format = time_format,
                           token = token,
                           source = source)

  if(httr2::resp_status(response)==200){

    data <- fetch_trends(query = query,
                         country = country,
                         region = region,
                         startDate = startDate,
                         endDate = endDate,
                         sentiment = sentiment,
                         is_news_related = is_news_related,
                         is_potentially_misleading = is_potentially_misleading,
                         spam = spam,
                         nsfw = nsfw,
                         lang = lang,
                         transcript_lang = transcript_lang,
                         ocr_lang = ocr_lang,
                         tags = tags,
                         type_label = type_label,
                         groupby = groupby,
                         time_format = time_format,
                         source = source,
                         token = token) %>%
      httr2::resp_body_json() %>%
      purrr::pluck('data') %>%
      tibble::enframe() %>%
      tidyr::unnest_wider('value', names_repair = 'minimal')

    return(data)

  }

  else {
    stop(httr2::resp_status_desc(response))
  }

}
