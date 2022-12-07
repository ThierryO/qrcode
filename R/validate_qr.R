#' @importFrom assertthat assert_that is.flag
#' @importFrom grDevices dev.off png
validate_qr <- function(x, ecl = c("L", "M", "Q", "H"), validate = TRUE) {
  ecl <- match.arg(ecl)
  assert_that(is.flag(validate), noNA(validate))
  requireNamespace("httr", quietly = TRUE)
  where <- tempfile(fileext = ".png")
  png(where)
  plot(qr_code(x, ecl))
  dev.off()
  on.exit(file.remove(where), add = TRUE, after = FALSE)
  result <- httr::POST(
    url = "http://api.qrserver.com/v1/read-qr-code/",
    body = list(file = httr::upload_file(where), outputformat = "json")
  )
  error <- httr::content(result)[[1]][["symbol"]][[1]][["error"]]
  assert_that(is.null(error), msg = error)
  tested <- httr::content(result)[[1]][["symbol"]][[1]][["data"]]
  if (!validate) {
    return(tested)
  }
  assert_that(
    tested == x, msg = paste0("Got '", tested, "' instead of '", x, "'")
  )
}
