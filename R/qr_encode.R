#' Create the bit encoding
#'
#' The message converted into a bit string.
#' @inheritParams qr_code
#' @examples
#' qr_encode("HELLO WORLD")
#' @export
#' @author Thierry Onkelinx
#' @family internal
qr_encode <- function(x, ecl = c("L", "M", "Q", "H")) {
  version <- qr_version(x = x, ecl = ecl)
  if (version$mode == "Numeric") {
    while (nchar(x) > 1 && substr(x, 1, 1) == "0") {
      x <- substring(x, 2)
    }
    version <- qr_version(x = x, ecl = ecl)
  }
  encoded <- switch(
    version$mode, Numeric = qr_encode_numeric(x),
    Alphanumeric = qr_encode_alnum(x), Byte = qr_encode_byte(x)
  )
  bit_string <- c(version$bit_string, encoded)
  return(bit_string)
}

qr_encode_numeric <- function(x) {
  groups <- character(0)
  while (nchar(x)) {
    groups <- c(groups, substr(x, 1, 3))
    x <- substring(x, 4)
  }
  bit_string <- vapply(
    groups, FUN.VALUE = vector(1, mode = "list"),
    FUN = function(i) {
      list(int2bits(as.integer(i), n_bit = c(4, 7, 10)[nchar(i)]))
    }
  )
  return(do.call(c, bit_string))
}

qr_encode_alnum <- function(x) {
  coded <- alphanum()[strsplit(x, "")[[1]]]
  if (length(coded) == 1) {
    return(int2bits(coded, 6))
  }
  code_mat <- head(coded, 2 * floor(length(coded) / 2))
  code_mat <- matrix(code_mat, nrow = 2)
  groups <- code_mat[1, ] * 45 + code_mat[2, ]
  bit_string <- vapply(
    groups, FUN.VALUE = vector("list", 1), FUN = function(i) {
      list(int2bits(i, 11))
    }
  )
  bit_string <- do.call(c, bit_string)
  if (length(coded) %% 2 == 0) {
    return(bit_string)
  }
  bits(c(bit_string, int2bits(tail(coded, 1), 6)))
}

qr_encode_byte <- function(x) {
  if (!Encoding(x) %in% c("unknown", "latin1")) {
    x <- iconv(x, Encoding(x), "latin1")
  }
  bits(as.logical(rev(rawToBits(rev(charToRaw(x))))))
}
