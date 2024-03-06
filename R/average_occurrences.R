#' Request Message and Chat Data
#'
#' @param message_data A tibble returned from palver::request_data or palver::request_messages
#'
#' @return The input tibble with aditional colunms on occurrences of the same text or midia and their average
#' @export

average_occurences <- function(message_data){

  message_data %>%
    dplyr::mutate(text_similarity_id = text %>%
                    stringr::str_to_lower(.) %>%
                    stringr::str_remove_all(pattern = ' ') %>%
                    stringi::stri_trans_general('Latin-ASCII'),
                  ocr_id = ocr %>%
                    stringr::str_to_lower(.) %>%
                    stringr::str_remove_all(pattern = ' ') %>%
                    stringi::stri_trans_general('Latin-ASCII'),
                  transcript_id = transcript %>%
                    stringr::str_to_lower(.) %>%
                    stringr::str_remove_all(pattern = ' ') %>%
                    stringi::stri_trans_general('Latin-ASCII')) %>%
    dplyr::group_by(text_similarity_id) %>%
    dplyr::mutate(text_similarity_id = dplyr::cur_group_id()) %>%
    dplyr::ungroup() %>%
    dplyr::group_by(ocr_id, transcript_id) %>%
    dplyr::mutate(media_similarity_id = dplyr::cur_group_id()) %>%
    dplyr::ungroup() %>%
    dplyr::mutate(text_similarity_id = dplyr::case_when(is.na(text) == F ~ text_similarity_id, T ~ NA),
                  media_similarity_id = dplyr::case_when(is.na(transcript) == F & is.na(ocr) == F ~ media_similarity_id, T ~ NA)) %>%
    dplyr::select(-.data$ocr_id,-.data$transcript_id) %>%
    dplyr::add_count(text_similarity_id) %>%
    dplyr::rename(occurences_text_similarity = 'n') %>%
    dplyr::add_count(media_similarity_id) %>%
    dplyr::rename(occurences_media_similarity = 'n') %>%
    dplyr::mutate(occurences_text_similarity = dplyr::case_when(is.na(text) == F ~ occurences_text_similarity, T ~ NA),
                  occurences_media_similarity = dplyr::case_when(is.na(transcript) == F & is.na(ocr) == F ~ occurences_media_similarity, T ~ NA)) %>%
    dplyr::mutate(avg_text_similarity = mean(occurences_text_similarity,na.rm=T),
                  avg_media_similarity = mean(occurences_media_similarity,na.rm=T))
}
