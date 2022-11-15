#' Generate a QR code for an event
#' @param start the required start time as `POSIXct`.
#' @param end the required end time as `POSIXct`.
#' @param title the required title of the event.
#' @param ... optional arguments as defined in the details.
#' @details
#' Optional arguments.
#' Other arguments are silently ignored.
#'
#' - `description`
#' - `location`
#' - `organiser`
#' - `url`
#' @inheritParams qr_code
#' @export
#' @importFrom assertthat assert_that is.string noNA
#' @family qr
qr_event <- function(
    start, end, title, ..., ecl = c("L", "M", "Q", "H")
) {
  ecl <- match.arg(ecl)
  assert_that(
    inherits(start, "POSIXct"), inherits(end, "POSIXct"), is.string(title),
    noNA(title)
  )
  dots <- list(...)
  dots <- dots[
    names(dots) %in% c("description", "location", "organiser", "url")
  ]
  strings <- vapply(dots, is.string, logical(1))
  assert_that(
    all(strings),
    msg = paste(
      "Some arguments are not a string:",
      paste(names(dots)[!strings], collapse = ", ")
    )
  )
  strings <- vapply(dots, noNA, logical(1))
  assert_that(
    all(strings),
    msg = paste(
      "Some arguments contains missing values:",
      paste(names(dots)[!strings], collapse = ", ")
    )
  )
  qr_message <- c(
    "BEGIN:VEVENT",
    paste0("DTSTAMP:", format(start, format = "%Y%m%dT%H%M%SZ", tz = "Z")),
    paste0("DTSTART:", format(start, format = "%Y%m%dT%H%M%SZ", tz = "Z")),
    paste0("DTEND:", format(end, format = "%Y%m%dT%H%M%S", tz = "Z")),
    paste0("SUMMARY:", title),
    paste(toupper(names(dots)), dots, sep = ":"),
    "END:VEVENT"
  )
  z <- qr_code(paste(qr_message, collapse = "\r\n"), ecl = ecl)
  class(z) <- c("qr_event", class(z))
  return(z)
}
