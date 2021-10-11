#' Function to generate polynomial
#'
#' @param ECcount error correction code word count
#'
#' @return polynomail to generate Error correction code
#' @author Victor Teh
#' @family legacy
#'

polynomialGenerator <- function(ECcount) { #nolint
  .Deprecated("qr_code")
  logTable <- create_log_table()

  poly <- c(0, 0)
  for (p in 2:ECcount) {
    tempPoly <- rep(0L, p)
    for (i in seq_len(p)) {
      if (i == p) {
        #(i 2)
        tempPoly[i]  <- lookup_log(logTable, ((poly[i] + p - 1L) %% 255))
      } else {
        #(i 2)(i+1 1)
        tempPoly[i] <- bitwXor(
          lookup_log(logTable, poly[i + 1]),
          lookup_log(logTable, (poly[i] + p - 1L) %% 255)
        )
      }
    }
    tempPoly[tempPoly > 255] <- tempPoly[tempPoly > 255] %% 255
    poly <- c(0, lookup_exponent(logTable, tempPoly))
    poly <- poly[poly != 255]
  }
  return(poly)
}
