#' @importFrom assertthat assert_that
#' @importFrom grDevices dev.off png
validate_qr <- function(x, ecl = c("L", "M", "Q", "H")) {
  ecl <- match.arg(ecl)
  where <- tempfile(fileext = ".png")
  assert_that(requireNamespace("httr"))
  png(where)
  plot(qr_code(x, ecl))
  dev.off()
  on.exit(file.remove(where), add = TRUE, after = FALSE)
  result <- httr::POST(
    url = "http://api.qrserver.com/v1/read-qr-code/",
    body = list(file = httr::upload_file(where), outputformat = "json")
  )
  all.equal(
    httr::content(result)[[1]][["symbol"]][[1]][["data"]],
    x
  )
}
