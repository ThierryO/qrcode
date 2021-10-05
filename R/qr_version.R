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
  list(
    integer(0), c(7L, 19L), c(7L, 23L), c(7L, 27L), c(7L, 31L), c(7L, 35L),
    c(7L, 23L, 39L), c(7L, 23L, 43L), c(7L, 27L, 47L), c(7L, 29L, 51L),
    c(7L, 31L, 55L), c(7L, 33L, 59L), c(7L, 35L, 63L), c(7L, 27L, 47L, 67L),
    c(7L, 27L, 49L, 71L), c(7L, 27L, 51L, 75L), c(7L, 27L, 51L, 75L),
    c(7L, 27L, 55L, 79L), c(7L, 27L, 57L, 83L), c(7L, 27L, 59L, 87L),
    c(7L, 35L, 63L, 91L), c(7L, 29L, 51L, 73L, 95L), c(7L, 27L, 51L, 75L, 99L),
    c(7L, 31L, 55L, 79L, 103L), c(7L, 29L, 55L, 81L, 107L),
    c(7L, 33L, 59L, 85L, 111L), c(7L, 31L, 59L, 87L, 115L),
    c(7L, 35L, 63L, 91L, 119L), c(7L, 27L, 51L, 75L, 99L, 123L),
    c(7L, 31L, 55L, 79L, 103L, 127L), c(7L, 27L, 53L, 79L, 105L, 131L),
    c(7L, 31L, 57L, 83L, 109L, 135L), c(7L, 35L, 61L, 87L, 109L, 139L),
    c(7L, 31L, 59L, 87L, 115L, 143L), c(7L, 35L, 63L, 91L, 119L, 147L ),
    c(7L, 31L, 55L, 79L, 103L, 127L, 151L),
    c(7L, 25L, 51L, 77L, 103L, 129L, 155L),
    c(7L, 29L, 55L, 81L, 107L, 133L, 159L),
    c(7L, 33L, 59L, 85L, 111L, 137L, 163L),
    c(7L, 27L, 55L, 83L, 111L, 139L, 167L),
    c(7L, 31L, 59L, 87L, 115L, 143L, 171L)
  )[[relevant$Version]] -> attr(bit_string, "alignment")
  return(
    list(
      version = relevant$Version, ecl = ecl, mode = qm, bit_string = bit_string
    )
  )
}
