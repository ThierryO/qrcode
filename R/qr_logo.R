#' Add a logo to a QR code
#'
#' First generate a `qr_code` with a higher `ecl` level.
#' Then add the logo.
#' The maximum area of logo depends on the difference in `ecl` level between the
#' version with and without logo.
#' The size of the logo is further restricted by its image ratio.
#' We shrink very wide or tall logos to make sure it still fits on the logo.
#' @param code A `qr_code` object
#' @param logo the path to a logo image file.
#' Must be either `png`, `svg` or `jpeg` format.
#' @param ecl the required error correction level for the QR code
#' after overlaying the logo.
#' Must be lower than the `ecl` in the `code`.
#' Defaults to `"L"`.
#' The difference between the `ecl` set here and the `ecl` in `code` determines
#' the maximum area of the logo.
#' For the largest logo, generate `code` with `ecl = "H"` and add the logo with
#' `ecl = "L"`.
#' @param hjust Horizontal position of the logo.
#' The default of `"c"` indicates the centre of the QR code.
#' Use `"r"` to align the right side of the logo with the right side of the QR
#' code.
#' Use `"l"` to align the left side of the logo with the right side of the two
#' vertical finder patterns.
#' @param vjust Vertical position of the logo.
#' The default of `"c"` indicates the centre of the QR code..
#' Use `"b"` to align the bottom of the logo with the bottom of the QR code.
#' Use `"t"` to align the top of the logo with the bottom side of the two
#' horizontal finder patterns.
#' @export
#' @importFrom assertthat assert_that
add_logo <- function(
  code, logo, ecl = c("L", "M", "Q", "H"), hjust = c("c", "l", "r"),
  vjust = c("c", "b", "t")
) {
  assert_that(inherits(code, "qr_code"))
  stopifnot("SEPA qr codes must be not obstructed" = !inherits(code, "qr_sepa"))
  ecl <- match.arg(ecl)
  hjust <- match.arg(hjust)
  vjust <- match.arg(vjust)
  error_level <- c(L = 0.07, M = 0.15, Q = 0.25, H = 0.3)
  error_in <- error_level[attr(code, "ecl")]
  error_out <- error_level[ecl]
  paste(
    "Your requested an QR code with logo with error level \"%s\" (%.0f%%).",
    "The input QR code has error level \"%s\" (%.0f%%).",
    "The error level of the input QR code must be higher than the version with",
    "the logo.",
    "Use the `ecl` argument of `qr_code()` and `add_logo()` to set the error",
    "level."
  ) |>
    sprintf(
      ecl, 100 * error_level[ecl], attr(code, "ecl"),
      100 * error_level[attr(code, "ecl")]
    ) -> ecl_error_message
  assert_that(error_in > error_out, msg = ecl_error_message)
  logo <- read_logo(logo = logo)
  attr(code, "logo") <- logo
  logo_ratio <- attr(logo, "height") / attr(logo, "width")
  attr(code, "logo_max_dim") <- ncol(code) -
    ifelse("c" %in% c(hjust, vjust), 22, 14)

  sqrt((error_in - error_out) * attr(code, "logo_max_dim") ^ 2 / logo_ratio) |>
    unname() |>
    min(attr(code, "logo_max_dim")) -> attr(code, "logo_width")
  attr(code, "logo_height") <- attr(code, "logo_width") * logo_ratio
  if (attr(code, "logo_height") > attr(code, "logo_max_dim")) {
    attr(code, "logo_width") <- attr(code, "logo_width") *
      attr(code, "logo_max_dim") / attr(code, "logo_height")
    attr(code, "logo_height") <- attr(code, "logo_max_dim")
  }
  attr(code, "logo_position") <- c(hjust, vjust)
  class(code) <- c("qr_logo", class(code))
  return(code)
}

#' @importFrom assertthat assert_that is.string noNA
#' @importFrom grDevices rgb
read_logo <- function(logo) {
  assert_that(is.string(logo), noNA(logo), file.exists(logo))
  gsub(".*\\.(.*?)", "\\1", logo) |>
    tolower() -> extension
  assert_that(
    extension %in% c("jpg", "jpeg", "png", "svg"),
    msg = "Currently only handles jpeg, png and svg logos"
  )
  mat <- switch(
    extension,
    png = {
      requireNamespace("png", quietly = TRUE)
      png::readPNG(logo, native = TRUE)
    },
    svg = {
      requireNamespace("rsvg", quietly = TRUE)
      rsvg::rsvg_nativeraster(logo)
    },
    {
      requireNamespace("jpeg", quietly = TRUE)
      jpeg::readJPEG(logo, native = TRUE)
    }
  )
  attr(mat, "type") <- ifelse(extension == "svg", "vector", "raster")
  attr(mat, "height") <- dim(mat)[1]
  attr(mat, "width") <- dim(mat)[2]
  attr(mat, "filename") <- logo
  return(mat)
}
