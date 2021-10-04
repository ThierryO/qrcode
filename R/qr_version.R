#' Determine the required version
#' @inheritParams qr_mode
#' @param ecl the required error correction level.
#' Available options are `"L"` (7%), `"M"` (15%), `"Q"` (25%) and `"H"` (30%)
#' @export
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
  count <- bits(as.logical(rev(rawToBits(as.raw(nchar(x))))))
  assert_that(
    length(count) <= count_length,
    msg = paste(
      "count longer than expected in qr_version().",
      "Please contact the maintainer and",
      "provide the code that triggered this error."
    )
  )
  bit_string <- c(bit_string, rep(FALSE, count_length - length(count)), count)
  remainder <- ifelse(
    relevant$Version < 14,
    ifelse(relevant$Version < 7, ifelse(relevant$Version < 2, 0, 7), 0),
    ifelse(
      relevant$Version < 28,
      ifelse(relevant$Version < 21, 3, 4),
      ifelse(relevant$Version < 35, 3, 0)
    )
  )
  list(
    NULL, c(6L, 18L), c(6L, 22L), c(6L, 26L), c(6L, 30L), c(6L, 34L),
    c(6L, 22, 38L), c(6L, 22L, 42L), c(6L, 26L, 46L), c(6L, 28L, 50L),
    c(6L, 30L, 54L), c(6L, 32L, 58L), c(6L, 34L, 62L), c(6L, 26L, 46L, 66L),
    c(6L, 26L, 48L, 70L), c(6L, 26L, 50L, 74L), c(6L, 26L, 50L, 74L),
    c(6L, 26L, 54L, 78L), c(6L, 26L, 56L, 82L), c(6L, 26L, 58L, 86L),
    c(6L, 34L, 62L, 90L), c(6L, 28L, 50L, 72L, 94L), c(6L, 26L, 50L, 74L, 98L),
    c(6L, 30L, 54L, 78L, 102L), c(6L, 28L, 54L, 80L, 106L),
    c(6L, 32L, 58L, 84L, 110L), c(6L, 30L, 58L, 86L, 114L),
    c(6L, 34L, 62L, 90L, 118L), c(6L, 26L, 50, 74L, 98L, 122L),
    c(6L, 30L, 54L, 78L, 102L, 126L), c(6L, 26L, 52L, 78L, 104L, 130L),
    c(6L, 30L, 56L, 82L, 108L, 134L), c(6L, 34L, )
  )
  attr(bit_string, "version") <- relevant$Version
  attr(bit_string, "ecl") <- ecl
  attr(bit_string, "dcword1") <- relevant$DCinGrp1
  attr(bit_string, "n1") <- relevant$Grp1
  attr(bit_string, "dcword2") <- relevant$DCinGrp2
  attr(bit_string, "n2") <- relevant$Grp2
  attr(bit_string, "ecword") <- relevant$ECwordPerBlock
  attr(bit_string, "remainder") <- remainder
  return(
    list(
      version = relevant$Version, ecl = ecl, mode = qm, bit_string = bit_string
    )
  )
}
