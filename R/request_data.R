#' Request Message and Chat Data
#'
#' @param query Query in lucene's syntax
#' @param country Country ISO code
#' @param region Region code
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
#' @param token Object with your token from function get_token
#'
#' @return A tibble from data required
#' @export


request_data <- function(
    query,
    country = NULL,
    region = NULL,
    startDate,
    endDate,
    sentiment = NULL,
    source = c('whatsapp', 'telegram', 'press', 'news', 'radio.medias', 'reddit', 'youtube', 'twitter'),
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

  source <- match.arg(source)
  sortField <- match.arg(sortField)
  sortOrder <- match.arg(sortOrder)


  messages_data <- palver::request_messages(query = query,
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
                                    token = token)
  if(nrow(messages_data) > 0){

    total_chat <- messages_data %>%
      dplyr::distinct(.data$chat_id) %>%
      dplyr::count() %>%
      purrr::pluck(1)

    query_chat <- purrr::map(.x = seq(1,total_chat,100),
                             .f = ~ stringr::str_c('id:',
                                                   stringr::str_c(unlist(messages_data %>%
                                                            dplyr::distinct(.data$chat_id) %>%
                                                            dplyr::slice(.x:(99+.x))),
                                                   collapse=" OR id:")))


    chats_data <- purrr::map(.x = query_chat,
                                 ~ palver::request_chats(query = .x,
                                                 source = source,
                                                 token = token) %>%
                               dplyr::select(-1) %>%
                               dplyr::rename(chat_name = .data$name,
                                             chat_id = .data$id)) %>%
      purrr::reduce(dplyr::bind_rows)

    data <- messages_data %>%
      dplyr::left_join(chats_data, by = c('chat_id','source'))

    return(data)}

  else stop('No documents found')

}
