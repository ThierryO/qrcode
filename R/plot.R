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
#' @importFrom graphics image par
#' @author Thierry Onkelinx
#' @family qr
#' @rdname plot.qr_code
plot.qr_logo <- function(x, col = c("white", "black"), y, ...) {
  z <- t(x)[, rev(seq_len(ncol(x)))]
  logo <- attr(x, "logo")
  logo <- t(logo)[, rev(seq_len(nrow(logo)))]

  if (nrow(logo) < ncol(logo)) {
    logo <- rbind(
      matrix(
        NA, nrow = floor((ncol(logo) - nrow(logo)) / 2), ncol = ncol(logo)
      ),
      logo,
      matrix(
        NA, nrow = ceiling((ncol(logo) - nrow(logo)) / 2), ncol = ncol(logo)
      )
    )
  } else {
    logo <- cbind(
      matrix(
        NA, ncol = floor((nrow(logo) - ncol(logo)) / 2), nrow = nrow(logo)
      ),
      logo,
      matrix(
        NA, ncol = ceiling((nrow(logo) - ncol(logo)) / 2), nrow = nrow(logo)
      )
    )
  }

  max_dim <- ncol(x) - 14
  scale <- (max_dim) / max(attr(x, "logo_width"), attr(x, "logo_height"))
  extra <- (scale - 1) * nrow(logo) / 2
  logo <- cbind(
    matrix(NA, ncol = ceiling(extra), nrow = nrow(logo)), logo,
    matrix(NA, ncol = floor(extra), nrow = nrow(logo))
  )
  logo <- rbind(
    matrix(NA, nrow = ceiling(extra), ncol = ncol(logo)), logo,
    matrix(NA, nrow = floor(extra), ncol = ncol(logo))
  )
  logo <- cbind(
    matrix(NA, ncol = ceiling(ncol(logo) * 3 / max_dim), nrow = nrow(logo)),
    logo,
    matrix(NA, ncol = ceiling(ncol(logo) * 11 / max_dim), nrow = nrow(logo))
  )
  logo <- rbind(
    matrix(NA, nrow = ceiling(nrow(logo) * 11 / max_dim), ncol = ncol(logo)),
    logo,
    matrix(NA, nrow = ceiling(nrow(logo) * 3 / max_dim), ncol = ncol(logo))
  )

  old_par <- par(no.readonly = TRUE)
  on.exit(par(old_par), add = TRUE, after = FALSE)
  par(mai = rep(0, 4), mar = rep(0, 4))
  image(z, asp = 1, col = col, axes = FALSE)
  image(logo, col = attr(attr(x, "logo"), "colour"), add = TRUE)
}
