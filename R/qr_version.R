#' Determine the required version
#' @inheritParams qr_mode
#' @param ecl the required error correction level.
#' Available options are `"L"` (7%), `"M"` (15%), `"Q"` (25%) and `"H"` (30%)
#' @export
qr_version <- function(x, ecl = c("L", "M", "Q", "H")) {
  ecl <- match.arg(ecl)
  relevant <- qrCodeSpec[qrCodeSpec$ECL == ecl, ]
  qm <- qr_mode(x)
  byte_string <- list(
    Numeric = c(FALSE, FALSE, FALSE, TRUE),
    Alphanumeric = c(FALSE, FALSE, TRUE, FALSE),
    Byte = c(FALSE, TRUE, FALSE, FALSE)
  )[[qm]]
  relevant <- relevant[head(which(nchar(x) <= relevant[[qm]]), 1), ]
  indicator <- matrix(
    c(10, 12, 14, 9, 11, 13, 8, 16, 16), nrow = 3,
    dimnames = list(NULL, c("Numeric", "Alphanumeric", "Byte"))
  )
  count_length <- indicator[cut(relevant$Version, c(0, 9, 26, 40)), qm]
  count <- as.logical(rev(rawToBits(as.raw(nchar(x)))))
  count <- tail(count, max(which(rev(count))))
  byte_string <- c(byte_string, rep(FALSE, count_length - length(count)), count)
  return(
    list(
      version = relevant$Version, ecl = ecl, mode = qm,
      byte_string = byte_string
    )
  )
}
