#' Error correction code generator
#' Generate error correction code based on the input polynomial.
#'
#' @param GenPoly generated polynomial to calculate error correction code word
#' @param DataPoly input data polynomial
#' @param DCWordCount data code word count
#' @param ECWordCount error code word count
#'
#' @return Error code word polynomial
#' @export
#' @importFrom utils tail

ECgenerator <- function(GenPoly, DataPoly, DCWordCount, ECWordCount) { #nolint
  logTable <- create_log_table()

  targetDataPoly <- c(DataPoly, rep(0, ECWordCount))
  for (j in seq_len(DCWordCount)) {
    if (targetDataPoly[1] > 0) {
      poly_temp <- GenPoly + lookup_exponent(logTable, targetDataPoly[1])
      poly_temp <- poly_temp %% 255
      poly_temp <- lookup_log(logTable, poly_temp)
      targetDataPoly[seq_along(poly_temp)] <- bitwXor(
        targetDataPoly[seq_along(poly_temp)], poly_temp
      )
    }
    targetDataPoly <- tail(targetDataPoly, -1)
  }
  return(targetDataPoly)
}

create_log_table <- function() {
  log_table <- data.frame(exponent = 0:254, log = 1L)
  for (i in 1:254) {
    temp <- 2L * log_table$log[i]
    log_table$log[i + 1] <- ifelse(temp > 255, bitwXor(temp, 285), temp)
  }
  return(log_table)
}

lookup_log <- function(log_table, exponent) {
  log_table$log[exponent + 1]
}

lookup_exponent <- function(log_table, log) {
  idx <- vapply(
    log,
    function(x) {
      which(log_table$log == x)
    },
    integer(1)
  )
  log_table$exponent[idx]
}
