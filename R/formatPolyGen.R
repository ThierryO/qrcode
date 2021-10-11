#' Function to calculate and generate format polynomial
#'
#' @param formatString QRcode format binary string
#' @param polyString polynomial to create ECL for formatString
#' @author Victor Teh
#' @family legacy
#'
#'

formatPolyGen <- function(formatString, polyString) {
  .Deprecated("qr_code")

  formatString <- as.integer(unlist(strsplit(formatString, split = "")))
  oriFormatString <- formatString
  polyString <- as.integer(unlist(strsplit(polyString, split = "")))
  formatString <- c(formatString, rep(0, 10))

  for (i in 1:5) {
    if (formatString[1] == 0) {
      formatString <- formatString[2:length(formatString)]
    } else {
      polyTemp <- c(
        polyString, rep(0, length(formatString) - length(polyString))
      )
      formatString <- bitwXor(formatString, polyTemp)
      formatString <- formatString[2:length(formatString)]
    }
  }
  formatString <- c(oriFormatString, formatString)
  finalPoly <- as.integer(unlist(strsplit("101010000010010", split = "")))
  return(bitwXor(formatString, finalPoly))
}
