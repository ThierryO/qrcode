#' Extract coordinates from a QR code object
#' Selects the dark elements from the `qr_code` object and returns their
#' coordinates.
#' @param x the `qr_code` object.
#' @returns A `data.frame` with the `column` and `row` number of the dark
#' elements.
#' @examples
#' x <- qr_code("test")
#' plot(x)
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
