#' Determine the required version
#' Returns a list with the version, error correction level and mode.
#' The bit string encodes mode and the length of the input string.
#' @inheritParams qr_code
#' @examples
#' qr_version("HELLO WORLD")
#' qr_version("hello world", ecl = "H")
#' @export
#' @author Thierry Onkelinx
#' @family internal
qr_version <- function(x, ecl = c("L", "M", "Q", "H")) {
  ecl <- match.arg(ecl)
  relevant <- qrCodeSpec[qrCodeSpec$ECL == ecl, ]
  qm <- qr_mode(x)
  bit_string <- list(
    Numeric = bits(c(FALSE, FALSE, FALSE, TRUE)),
    Alphanumeric = bits(c(FALSE, FALSE, TRUE, FALSE)),
    Byte = bits(c(FALSE, TRUE, FALSE, FALSE))
  )[[qm]]
  relevant <- relevant[head(which(nchar(x) <= relevant[[qm]]), 1), ]
  indicator <- matrix(
    c(10, 12, 14, 9, 11, 13, 8, 16, 16), nrow = 3,
    dimnames = list(NULL, c("Numeric", "Alphanumeric", "Byte"))
  )
  count_length <- indicator[cut(relevant$Version, c(0, 9, 26, 40)), qm]
  count <- bits(as.logical(rev(head(intToBits(nchar(x)), count_length))))
  bit_string <- c(bit_string, count)
  remainder <- ifelse(
    relevant$Version < 14,
    ifelse(relevant$Version < 7, ifelse(relevant$Version < 2, 0, 7), 0),
    ifelse(
      relevant$Version < 28,
      ifelse(relevant$Version < 21, 3, 4),
      ifelse(relevant$Version < 35, 3, 0)
    )
  )
  attr(bit_string, "version") <- relevant$Version
  attr(bit_string, "ecl") <- ecl
  attr(bit_string, "dcword1") <- relevant$DCinGrp1
  attr(bit_string, "n1") <- relevant$Grp1
  attr(bit_string, "dcword2") <- relevant$DCinGrp2
  attr(bit_string, "n2") <- relevant$Grp2
  attr(bit_string, "ecword") <- relevant$ECwordPerBlock
  attr(bit_string, "remainder") <- remainder
  attr(bit_string, "alignment") <- list(
    "1" = numeric(0), "2" = c(6, 18), "3" = c(6, 22), "4" = c(6, 26),
    "5" = c(6, 30), "6" = c(6, 34), "7" = c(6, 22, 38), "8" = c(6, 24, 42),
    "9" = c(6, 26, 46), "10" = c(6, 28, 50), "11" = c(6, 30, 54),
    "12" = c(6, 32, 58), "13" = c(6, 34, 62), "14" = c(6, 26, 46, 66),
    "15" = c(6, 26, 48, 70), "16" = c(6, 26, 50, 74), "17" = c(6, 30, 54, 78),
    "18" = c(6, 30, 56, 82), "19" = c(6, 30, 58, 86), "20" = c(6, 34, 62, 90),
    "21" = c(6, 28, 50, 72, 94), "22" = c(6, 26, 50, 74, 98),
    "23" = c(6, 30, 54, 78, 102), "24" = c(6, 28, 54, 80, 106),
    "25" = c(6, 32, 58, 84, 110), "26" = c(6, 30, 58, 86, 114),
    "27" = c(6, 34, 62, 90, 118), "28" = c(6, 26, 50, 74, 98, 122),
    "29" = c(6, 30, 54, 78, 102, 126), "30" = c(6, 26, 52, 78, 104, 130),
    "31" = c(6, 30, 56, 82, 108, 134), "32" = c(6, 34, 60, 86, 112, 138),
    "33" = c(6, 30, 58, 86, 114, 142), "34" = c(6, 34, 62, 90, 118, 146),
    "35" = c(6, 30, 54, 78, 102, 126, 150),
    "36" = c(6, 24, 50, 76, 102, 128, 154),
    "37" = c(6, 28, 54, 80, 106, 132, 158),
    "38" = c(6, 32, 58, 84, 110, 136, 162),
    "39" = c(6, 26, 54, 82, 110, 138, 166),
    "40" = c(6, 30, 58, 86, 114, 142, 170)
  )[[relevant$Version]]
  attr(bit_string, "alignment") <- as.integer(attr(bit_string, "alignment")) + 1
  return(
    list(
      version = relevant$Version, ecl = ecl, mode = qm, bit_string = bit_string
    )
  )
}
