#' Plot the QR code
#' @param x the `qr_code` object
#' @param col Define the colours.
#' The first element refers to `FALSE` ann the second `TRUE`.
#' Defaults to `c("white", "black")`.
#' @param y currently ignored
#' @param ... currently ignored
#' @export
#' @importFrom graphics image par
plot.qr_code <- function(x, col = c("white", "black"), y, ...) {
  z <- t(x)[, rev(seq_len(ncol(x)))]
  old_par <- par(no.readonly = TRUE)
  on.exit(par(old_par))
  par(mai = rep(0, 4), mar = rep(0, 4))
  image(z, asp = 1, col = col, axes = FALSE)
}