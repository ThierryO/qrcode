#' Print the qr_code object
#'
#' Please use `plot(x)` for a better quality image
#' @param x the `qr_code` object
#' @param ... currently ignored
#' @examples
#' qr_code("HELLO WORLD")
#' @export
#' @author Thierry Onkelinx
#' @family qr
print.qr_code <- function(x, ...) {
  x <- cbind(x, FALSE)
  x <- rbind(x, FALSE)
  upper_left <- x[seq(1, ncol(x), by = 2), seq(1, ncol(x), by = 2)]
  upper_right <- x[seq(1, ncol(x), by = 2), seq(2, ncol(x), by = 2)]
  lower_left <- x[seq(2, ncol(x), by = 2), seq(1, ncol(x), by = 2)]
  lower_right <- x[seq(2, ncol(x), by = 2), seq(2, ncol(x), by = 2)]
  blocks <- ifelse(
    upper_left,
    ifelse(
      upper_right,
      ifelse(
        lower_left,
        ifelse(lower_right, "\u2588", "\u259B"),
        ifelse(lower_right, "\u259C", "\u2580")
      ),
      ifelse(
        lower_left,
        ifelse(lower_right, "\u2599", "\u258C"),
        ifelse(lower_right, "\u259A", "\u2598")
      )
    ),
    ifelse(
      upper_right,
      ifelse(
        lower_left,
        ifelse(lower_right, "\u259F", "\u259E"),
        ifelse(lower_right, "\u2590", "\u259D")
      ),
      ifelse(
        lower_left,
        ifelse(lower_right, "\u2584", "\u2596"),
        ifelse(lower_right, "\u2597", " ")
      )
    )
  )
  cat(apply(blocks, 1, paste, collapse = ""), sep = "\n")
  cat("\nuse plot() for a better quality image\n")
}
