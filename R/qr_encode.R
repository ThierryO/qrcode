#' Create the bit encoding
#' @inheritParams qr_mode
#' @inheritParams qr_version
#' @export
qr_encode <- function(x, ecl = c("L", "M", "Q", "H")) {
  version <- qr_version(x = x, ecl = ecl)
  encoded <- switch(
    version$mode, Numeric = qr_encode_numeric(x),
    Alphanumeric = qr_encode_alnum(x), Byte = qr_encode_byte(x)
  )
  c(version$byte_string, encoded)
}

qr_encode_numeric <- function(x) {
  groups <- character()
  while (nchar(x)) {
    groups <- c(groups, substr(x, 1, 3))
    x <- substring(x, 4)
  }
  groups <- as.integer(groups)
  bits <- vapply(
    groups, FUN.VALUE = vector(1, mode = "list"),
    FUN = function(i) {
      bits <- head(intToBits(i), ifelse(i < 10, 4, ifelse(i < 100, 7, 100)))
      list(as.logical(rev(bits)))
    }
  )
  return(unlist(bits))
}

qr_encode_alnum <- function(x) {
  coded <- alphanum()[strsplit(x, "")[[1]]]
  code_mat <- head(coded, 2 * floor(length(coded) / 2))
  code_mat <- matrix(code_mat, nrow = 2)
  groups <- code_mat[1, ] * 45 + code_mat[2, ]
  bits <- sapply(groups, intToBits)[11:1, ]
  bitstring <- as.logical(as.vector(bits))
  if (length(coded) %% 2 == 0) {
    return(bitstring)
  }
  c(bitstring, as.logical(intToBits(tail(coded, 1))[6:1]))
}

qr_encode_byte <- function(x) {
  as.logical(rev(rawToBits(rev(charToRaw(x)))))
}
