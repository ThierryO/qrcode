#' Determine the required mode
#'
#' The current implementation handles three modes: numeric, alphanumeric and
#' byte.
#' Kanji is currently not supported.
#' Please contact the maintainer if you need it.
#' Numeric: only digits from 0 to 9
#' Alphanumeric: all numeric characters, upper case `LETTERS`, and the
#' characters `" "` (space), `"$"`, `"%"`, `"*"`, `"+"`, `"-"`, `"."`, `"/"` and
#' `":"`
#' Byte: All characters from the Latin 1 (ISO 8859-1) character set.
#' Input strings with an other encoding are converted into Latin 1.
#' The function return an error if such conversion fails.
#'
#' @inheritParams qr_code
#' @examples
#' qr_mode("0123")
#' qr_mode("A")
#' qr_mode("a")
#' @return a character indicating the mode
#' @export
#' @importFrom assertthat assert_that is.string noNA
#' @author Thierry Onkelinx
#' @family internal
qr_mode <- function(x) {
  assert_that(is.string(x), noNA(x))
  if (grepl("^[0-9]*$", x)) {
    return("Numeric")
  }
  regexp <- paste(c("^[", names(alphanum()), "]*$"), collapse = "")
  if (grepl(regexp, x)) {
    return("Alphanumeric")
  }
  if (!Encoding(x) %in% c("unknown", "latin1")) {
    x <- iconv(x, Encoding(x), "latin1")
  }
  assert_that(
    Encoding(x) %in% c("unknown", "latin1"), noNA(x),
    msg = "current version handles only latin1 characters"
  )
  return("Byte")
}

alphanum <- function() {
  structure(
    0:44, .Names = c(0:9, LETTERS, " ", "$", "%", "*", "+", "-", ".", "/", ":")
  )
}
