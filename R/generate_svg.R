#' Generate the QR code as an svg file
#'
#' Create the QR code using [qrcode_gen()] and save it as an svg file.
#' @param string Input string for the QR code.
#'   Passed to `dataString` of [qrcode_gen()].
#' @param filename Where to store the filename.
#'   Silently overwrites existings files.
#'   Tries to create the path, when it doesn't exist.
#' @param size size of the svg file in pixels.
#' @param foreground Stroke and fill colour for the foreground.
#'   Use a valid [CSS color](https://www.w3schools.com/colors/).
#'   Default to `"black"`.
#' @param background Fill colour for the background.
#'   Use a valid [CSS color](https://www.w3schools.com/colors/).
#'   Default to `"white"`.
#' @param show Open the file after creating it.
#'   Defaults to `TRUE` on [interactive()] sessions, otherwise `FALSE`.
#' @param error_correction Required error correction level.
#'   Passed to `ErrorCorrectionLevel` of [qrcode_gen()].
#' @return invisible `NULL`
#' @export
#' @importFrom utils browseURL
generate_svg <- function(
  string, filename, size = 100, foreground = "black", background = "white",
  show = interactive(), error_correction = c("L", "M", "H", "Q")
) {
  error_correction <- match.arg(error_correction)
  heading <- c(
    "<?xml version=\"1.0\" encoding=\"UTF-8\" ?>",
    sprintf(
      paste(
        "<svg xmlns=\"http://www.w3.org/2000/svg\"",
        "xmlns:xlink=\"http://www.w3.org/1999/xlink\"",
        "height=\"%1$i\" width=\"%1$i\">"
      ),
      size
    ),
    sprintf("  <g id=\"qrcode:%s\">", string),
    sprintf(
      paste(
        "    <rect x=\"0\" y=\"0\" width=\"%1$ipx\" height=\"%1$ipx\"",
        "style=\"fill:%2$s;\"/>"
      ),
      size, background
    )
  )
  footing <- c("  </g>", "</svg>")
  qrcode <- qrcode_gen(
    dataString = string, ErrorCorrectionLevel = error_correction,
    dataOutput = TRUE, plotQRcode = FALSE
  )
  pixel <- size / ncol(qrcode)
  top_left <- (which(qrcode == 1, arr.ind = TRUE) - 1) * pixel
  svg_data <- sprintf(
    paste(
      "    <rect x=\"%.2f\" y=\"%.2f\" width=\"%3$0.2f\" height=\"%3$0.2f\"",
      "stroke=\"%4$s\" stroke-width=\"0.2\" stroke-linecap=\"butt\"",
      "fill=\"%4$s\"/>"
    ),
    top_left[, 1], top_left[, 2], pixel, foreground
  )
  dir.create(dirname(filename), showWarnings = FALSE, recursive = TRUE)
  writeLines(c(heading, svg_data, footing), filename)
  if (show) {
    browseURL(filename)
  }
  return(invisible(NULL))
}
