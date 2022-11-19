#' Generate QR code with a logo on top
#'
#' Will create an `svg` file with the QR code and logo.
#' @param logo the path to a logo image file
#' @inheritParams qr_code
#' @inheritParams generate_svg
#' @importFrom assertthat assert_that is.string
#' @importFrom knitr image_uri
qr_logo <- function(
  x, logo, filename, size = 300, foreground = "black", background = "white",
  show = interactive()
) {
  assert_that(is.string(x))
  assert_that(file.exists(logo))
  ecl <- "L"
  input_ecl <- c(L = "H")[ecl]
  qr_code(x = x, ecl = input_ecl) |>
    generate_svg(
      filename = filename, size = size, foreground = foreground,
      background = background, show = TRUE
    )
  svg <- readLines(filename)
  n_svg <- length(svg)
  error_level <- c(L = 0.07, M = 0.15, Q = 0.25, H = 0.3)
  logo_width <- (error_level[input_ecl] - error_level[ecl]) * size
  img <- paste(
    "  <image href = \"%1$s\" x = \"%2$.0f\" y = \"%2$.0f\"",
    "width = \"%3$.0f\" height = \"%3$.0f\" />"
  ) |>
    sprintf(image_uri(logo), (size - logo_width) / 2, logo_width)
  svg[-n_svg] |>
    c(img, svg[n_svg]) |>
    writeLines(filename)
  if (show) {
    browseURL(filename)
  }
  return(filename)
}
