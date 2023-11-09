#' Extract coordinates from a QR code object.
#'
#' Selects the dark elements from the `qr_code` object and returns their
#' coordinates.
#' This can be useful when you want to create a QR code with a custom style.
#' @param x the `qr_code` object.
#' @returns A `data.frame` with the `column` and `row` number of the dark
#' elements.
#' @examples
#' x <- qr_code("test")
#' plot(x)
#' head(coordinates(x))
#' plot(coordinates(x), pch = 19, cex = 2, asp = 1)
#' @author Thierry Onkelinx
#' @family qr
#' @export
#' @importFrom assertthat assert_that
coordinates <- function(x) {
  assert_that(inherits(x, "qr_code"))
  data.frame(
    column = rep(seq_len(nrow(x)), ncol(x)),
    row = rep(rev(seq_len(ncol(x))), each = nrow(x))
  )[as.vector(x), ]
}
