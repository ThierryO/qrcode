#' Create a QR code for a location
#' @param latitude the latitude of the location.
#' @param longitude the longitude of the location.
#' @inheritParams qr_code
#' @export
#' @importFrom assertthat assert_that is.number noNA
#' @family qr
#' @examples
#' qr_location(50.8449861, 4.3499932) |>
#'   plot()
qr_location <- function(latitude, longitude, ecl = c("L", "M", "Q", "H")) {
  assert_that(
    is.number(latitude), is.number(longitude), noNA(latitude), noNA(longitude),
    latitude >= -90, latitude <= 90, longitude >= -180, longitude <= 180
  )
  sprintf("geo:%1$.7f,%2$.7f?%1$.7f,%2$.7f", latitude, longitude) |>
    qr_code(ecl = ecl) -> output
  class(output) <- c("qr_geo", class(output))
  return(output)
}
