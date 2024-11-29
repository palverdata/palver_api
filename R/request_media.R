#' Request Media
#'
#' @param media_path Media path from messages data
#' @param download Logical parameter for downloading
#' @param path Path file to save media
#' @param file_name File name to save media
#' @param source Source of messages data
#' @param token Token access
#'
#' @return Download media or return its URL download path
#' @export
#'

request_media <- function(
    media_path,
    download = c(TRUE,FALSE),
    path,
    file_name = 'media',
    source = c('whatsapp', 'telegram', 'reddit', 'tiktok', 'press', 'news', 'radio.medias', 'television', 'youtube', 'twitter'),
    token){

  source <- match.arg(source)

  url <- stringr::str_c('https://mercury-api.anax.com.br/api/', source, '/medias')

  parameters <- list('media_path' = media_path)

  response <- httr2::request(url) %>%
    httr2::req_method('POST') %>%
    httr2::req_auth_bearer_token(token) %>%
    httr2::req_headers('Accept' = 'application/json',
                       'Content-Type' = 'application/json') %>%
    httr2::req_body_json(data = parameters) %>%
    httr2::req_perform()


  if (httr2::resp_status(response) == 200) {

    url_media <- httr2::resp_body_json(response) %>%
      purrr::pluck('data') %>%
      purrr::pluck('url')

    type <- httr2::resp_body_json(response) %>%
      purrr::pluck('data') %>%
      purrr::pluck('mime_type') %>%
      stringr::str_extract(pattern = '(?<=\\/).*')

    if(download == T){

      utils::download.file(url = url_media,
                           destfile = stringr::str_c(path, source, '_', file_name, '.', type),
                           method = 'curl')
    }

    else return(url_media)

  }
  else stop(httr2::resp_status_desc(response))
}
