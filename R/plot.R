#' Plot the QR code
#' @param x the `qr_code` object
#' @param col Define the colours.
#' The first element refers to `FALSE` and the second `TRUE`.
#' Defaults to `c("white", "black")`.
#' @param y currently ignored
#' @param ... currently ignored
#' @examples
#' qr <- qr_code("HELLO WORLD")
#' plot(qr)
#' @export
#' @importFrom graphics image par
#' @author Thierry Onkelinx
#' @family qr
plot.qr_code <- function(x, col = c("white", "black"), y, ...) {
  z <- t(x)[, rev(seq_len(ncol(x)))]
  old_par <- par(no.readonly = TRUE)
  on.exit(par(old_par), add = TRUE, after = FALSE)
  par(mai = rep(0, 4), mar = rep(0, 4))
  image(z, asp = 1, col = col, axes = FALSE)
}

#' @export
#' @importFrom grDevices as.raster
#' @importFrom graphics par rasterImage
#' @author Thierry Onkelinx
#' @family qr
#' @rdname plot.qr_code
plot.qr_logo <- function(x, col = c("white", "black"), y, ...) {
  code <- matrix(col[1], ncol = ncol(x), nrow = nrow(x))
  code[x] <- col[2]
  code <- as.raster(code)

  old_par <- par(no.readonly = TRUE)
  on.exit(par(old_par), add = TRUE, after = FALSE)
  par(mai = rep(0, 4), mar = rep(0, 4))
  plot(NULL, xlim = c(0, 1), ylim = c(0, 1), axes = FALSE, asp = 1)
  min_pos <- max(par()$usr[c(1, 3)])
  max_pos <- min(par()$usr[c(2, 4)])
  rasterImage(code, min_pos, min_pos, max_pos, max_pos, interpolate = FALSE)

  logo <- attr(x, "logo")
  vertical <- switch(
    attr(x, "logo_position")[2],
    c = c(0, attr(x, "logo_height")) +
      (ncol(code) - attr(x, "logo_height")) / 2,
    b = 3 + c(0, attr(x, "logo_height")),
    t = ncol(code) - 11 - c(attr(x, "logo_height"), 0)
  ) / ncol(code)
  horizontal <- switch(
    attr(x, "logo_position")[1],
    c = c(0, attr(x, "logo_width")) + (ncol(code) - attr(x, "logo_width")) / 2,
    r = ncol(code) - 3 - c(attr(x, "logo_width"), 0),
    l = c(0, attr(x, "logo_width")) + 11
  ) / ncol(code)

  vertical <- (max_pos - min_pos) * vertical + min_pos
  horizontal <- (max_pos - min_pos) * horizontal + min_pos
  rasterImage(logo, horizontal[1], vertical[1], horizontal[2], vertical[2])
}
