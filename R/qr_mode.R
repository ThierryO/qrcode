#' Determine the required mode
#' @param x the input string
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
