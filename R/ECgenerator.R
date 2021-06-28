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

ECgenerator <- function(GenPoly, DataPoly, DCWordCount, ECWordCount) { #nolint

  #antilog table
  logTable <- data.frame(exponent = 0:255, log = 1)
  for (i in 1:255) {
    temp <- 2 * logTable$log[i]
    logTable$log[i + 1] <- ifelse(temp > 255, bitwXor(temp, 285), temp)
  }

  targetDataPoly <- c(DataPoly, rep(0, ECWordCount))
  for (j in seq_len(DCWordCount)) {
    if (targetDataPoly[1] > 0) {
      data_poly_temp <- unlist(
        lapply(
          targetDataPoly,
          function(x) {
            logTable[logTable$log == x, 1]
          }
        )
      )
      poly_temp <- GenPoly + data_poly_temp[1]
      poly_temp <- poly_temp %% 255
      poly_temp <- unlist(
        lapply(
          poly_temp,
          function(x) {
            logTable[logTable$exponent == x, 2]
          }
        )
      )
      targetDataPoly[seq_along(poly_temp)] <- bitwXor(
        targetDataPoly[seq_along(poly_temp)], poly_temp
      )
    }
    targetDataPoly <- targetDataPoly[2:length(targetDataPoly)]
  }
  return(targetDataPoly)
}
