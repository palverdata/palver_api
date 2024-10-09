#' Convert date into ISO date format
#'
#' @param day Day in format DD
#' @param month Month in format MM
#' @param year Year in format YYYY
#' @param hour Hour in format HH
#' @param min Minutes in format MM
#' @param sec Seconds in format SS
#' @param timezone Timezone in TZ identifier
#'
#' @return A datetime in ISO format
#' @export
#'
date_iso_format <- function(day,
                            month,
                            year,
                            hour = 0,
                            min = 0,
                            sec = 0,
                            timezone = 'UTC'){

  datetime <- lubridate::make_datetime(
    year = year,
    month = month,
    day = day,
    hour = hour,
    min = min,
    sec = sec,
    tz = timezone) |>
    lubridate::format_ISO8601(usetz = T)

  return(datetime)
}

