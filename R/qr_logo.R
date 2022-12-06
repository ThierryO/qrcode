#' Generate QR code with a logo on top
#'
#' Will create an `svg` file with the QR code and logo.
#' @inheritParams qr_code
#' @param logo the path to a logo image file
#' @param ecl the required error correction level for the remaining QR code
#' covered by the logo.
#' Available options are `"L"` (7%), `"M"` (15%), `"Q"` (25%) and `"H"` (30%).
#' Defaults to `"L"`.
#' @param input_ecl the required error correction level for the QR code
#' underneath the logo.
#' Must be higher than the selected level in `ecl`.
#' Defaults to `"H"`.
#' The difference between `ecl` and `input_ecl` determines the size of the logo.
#' @export
qr_logo <- function(
  x, logo, ecl = c("L", "M", "Q", "H"), input_ecl = c("H", "Q", "M", "L")
) {
  ecl <- match.arg(ecl)
  input_ecl <- match.arg(input_ecl)
  error_level <- c(L = 0.07, M = 0.15, Q = 0.25, H = 0.3)
  error_in <- error_level[input_ecl]
  error_out <- error_level[ecl]
  assert_that(
    error_in > error_out,
    msg = "error level in `input_ecl` must be higher than in `ecl`"
  )
  logo <- read_logo(logo = logo)
  logo_ratio <- attr(logo, "height") / attr(logo, "width")
  code <- qr_code(x = x, ecl = input_ecl)
  attr(code, "logo") <- logo
  max_dim <- ncol(code) - 14
  sqrt((error_in - error_out) * max_dim ^ 2 / logo_ratio) |>
    unname() |>
    min(max_dim) -> attr(code, "logo_width")
  attr(code, "logo_height") <- attr(code, "logo_width") * logo_ratio
  if (attr(code, "logo_height") > max_dim) {
    attr(code, "logo_width") <- attr(code, "logo_width") * max_dim /
      attr(code, "logo_height")
    attr(code, "logo_height") <- max_dim
  }
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
  switch(
    extension,
    png = {
      requireNamespace("png")
      original <- png::readPNG(logo)
      mat <- array(1, dim = c(nrow(original), ncol(original), 4))
      mat[, , 1:3] <- original
    },
    svg = {
      requireNamespace("rsvg")
      mat <- rsvg::rsvg(logo)
    },
    {
      requireNamespace("jpeg")
      original <- jpeg::readJPEG(logo)
      mat <- array(1, dim = c(nrow(original), ncol(original), 4))
      mat[, , 1:3] <- original
    }
  )
  rgb(
    red = as.vector(mat[, , 1]), green = as.vector(mat[, , 2]),
    blue = as.vector(mat[, , 3]), alpha = as.vector(mat[, , 4])
  ) |>
    factor() -> fmat
  fmat |>
    as.integer() |>
    matrix(ncol = ncol(mat), nrow = nrow(mat)) -> mat
  attr(mat, "type") <- ifelse(extension == "svg", "vector", "raster")
  attr(mat, "height") <- dim(mat)[1]
  attr(mat, "width") <- dim(mat)[2]
  attr(mat, "colour") <- levels(fmat)
  attr(mat, "filename") <- logo
  return(mat)
}
