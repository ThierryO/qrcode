#' Generate the QR code
#'
#' A [QR code](https://en.wikipedia.org/wiki/QR_code) is a two-dimensional
#' barcode developed by the Denso Wave company.
#' @param x the input string
#' @param ecl the required error correction level.
#' Available options are `"L"` (7%), `"M"` (15%), `"Q"` (25%) and `"H"` (30%).
#' Defaults to `"L"`.
#' @return The QR code as a logical matrix with "qr_code" class.
#' @examples
#' qr_code("https://www.r-project.org")
#' qr <- qr_code("https://cran.r-project.org/package=qrcode", ecl = "M")
#' qr
#' plot(qr)
#' # the qr_code object is a logical matrix
#' str(qr)
#' qr[1:10, 1:10]
#' @export
#' @author Thierry Onkelinx
#' @family qr
qr_code <- function(x, ecl = c("L", "M", "Q", "H")) {
  ecl <- match.arg(ecl)
  base_matrix <- qr_matrix(x = x, ecl = ecl)
  mask_patterns <- mask_pattern(code_dim = ncol(base_matrix$patterns))
  candidate <- vapply(
    0:7, FUN.VALUE = vector("list", 1), base_matrix = base_matrix,
    FUN = function(mask, base_matrix) {
      list(
        apply_mask(
          payload = base_matrix$payload, patterns = base_matrix$patterns,
          mask = mask, version = base_matrix$version,
          mask_pattern = mask_patterns, ecl = ecl
        )
      )
    }
  )
  est_penalty <- vapply(candidate, penalty, numeric(1))
  # select the lowest penalty
  best <- candidate[[head(order(est_penalty), 1)]]
  # add quiet zone
  best <- rbind(FALSE, FALSE, FALSE, best, FALSE, FALSE, FALSE)
  best <- cbind(FALSE, FALSE, FALSE, best, FALSE, FALSE, FALSE)
  class(best) <- c("qr_code", "matrix")
  attr(best, "string") <- x
  return(best)
}

mask_pattern <- function(code_dim, mask = 0:7) {
  coords <- expand.grid(x = seq_len(code_dim), y = seq_len(code_dim))
  # take into account that the rules use indices starting at 0
  # while R uses indices starting at 1
  coords$mask_0 <- (coords$x + coords$y) %% 2 == 0
  coords$mask_1 <- coords$x %% 2 == 1
  coords$mask_2 <- coords$y %% 3 == 1
  coords$mask_3 <- (coords$x + coords$y) %% 3 == 2
  coords$mask_4 <- (
    floor((coords$x - 1) / 2) + floor((coords$y - 1) / 3)
  ) %% 2 == 0
  coords$mask_5 <- (
    ((coords$x - 1) * (coords$y - 1)) %% 2 +
      ((coords$x - 1) * (coords$y - 1)) %% 3
  ) == 0
  coords$mask_6 <- (
    (((coords$x - 1) * (coords$y - 1)) %% 2 +
       ((coords$x - 1) * (coords$y - 1)) %% 3) %% 2
  ) == 0
  coords$mask_7 <- (
    (((coords$x - 1) + (coords$y - 1)) %% 2 +
       ((coords$x - 1) * (coords$y - 1)) %% 3) %% 2
  ) == 0
  return(coords)
}

#' @importFrom utils tail
format_string <- function(ecl, mask) {
  ecl_int <- as.integer(factor(ecl, levels = c("M", "L", "H", "Q"))) - 1
  version_bits <- int2bits(ecl_int * 8 + mask, n_bit = 5L)
  remainder <- c(version_bits, rep(FALSE, 10))
  generator <- int2bits(1335, n_bit = 11)
  while (!remainder[1] & length(remainder) > 10) {
    remainder <- tail(remainder, -1)
  }
  while (length(remainder) > 10) {
    divider <- c(generator, rep(FALSE, length(remainder) - length(generator)))
    remainder <- xor(remainder, divider)
    while (!remainder[1]) {
      remainder <- tail(remainder, -1)
    }
  }
  bit_string <- c(version_bits, rep(FALSE, 10 - length(remainder)), remainder)
  generator <- int2bits(21522, n_bit = 15L)
  bits(xor(bit_string, generator))
}

#' @importFrom utils tail
format_version <- function(version) {
  version_bits <- int2bits(version, 6)
  remainder <- c(version_bits, rep(FALSE, 12))
  generator <- int2bits(7973, 13)
  while (!remainder[1] & length(remainder) > 12) {
    remainder <- tail(remainder, -1)
  }
  while (length(remainder) > 12) {
    divider <- c(generator, rep(FALSE, length(remainder) - length(generator)))
    remainder <- xor(remainder, divider)
    while (!remainder[1]) {
      remainder <- tail(remainder, -1)
    }
  }
  c(version_bits, rep(FALSE, 12 - length(remainder)), remainder)
}

#' @importFrom assertthat assert_that is.number
apply_mask <- function(payload, patterns, mask, version, mask_pattern, ecl) {
  assert_that(is.number(mask), 0 <= mask, mask <= 7)
  mask_coord <- as.matrix(
    mask_pattern[mask_pattern[[sprintf("mask_%i", mask)]], c("x", "y")]
  )
  payload[mask_coord] <- !payload[mask_coord]
  payload[!is.na(patterns)] <- FALSE
  payload <- payload | patterns
  payload[is.na(payload)] <- FALSE
  fs <- format_string(ecl, mask)
  payload[9, 1:6] <- head(fs, 6)
  payload[9, 8:9] <- fs[7:8]
  payload[9, ncol(payload) - 7:0] <- tail(fs, 8)
  payload[ncol(payload) - 0:6, 9] <- head(fs, 7)
  payload[8, 9] <- fs[9]
  payload[6:1, 9] <- tail(fs, 6)
  if (version < 6) {
    return(payload)
  }
  fs <- format_version(version)
  fs <- matrix(rev(fs), nrow = 3)
  payload[ncol(payload) - 10:8, 1:6] <- fs
  payload[1:6, ncol(payload) - 10:8] <- t(fs)
  return(payload)
}

penalty <- function(payload) {
  penalty_1(payload) + penalty_2(payload) + penalty_3(payload) +
    penalty_4(payload)
}

penalty_1 <- function(payload) {
  # each group of five or more same-coloured modules in a row (or column)
  # 3 points per group + 1 additional point per modules above 5
  delta <- apply(payload, 1, diff)
  group_id <- rbind(0, apply(abs(delta), 2, cumsum))
  groups <- unlist(apply(group_id, 2, table))
  groups <- groups[groups >= 5] - 5
  horizontal <- sum(groups) + 3 * length(groups)
  # vertical
  delta <- apply(payload, 2, diff)
  group_id <- rbind(0, apply(abs(delta), 2, cumsum))
  groups <- unlist(apply(group_id, 2, table))
  groups <- groups[groups >= 5] - 5
  vertical <- sum(groups) + 3 * length(groups)
  return(horizontal + vertical)
}

penalty_2 <- function(payload) {
  # 3 times the number of 2x2 modules with the same colour
  horizontal <- payload[-ncol(payload), -ncol(payload)] ==
    payload[-ncol(payload), -1]
  vertical <- payload[-ncol(payload), -ncol(payload)] ==
    payload[-1, -ncol(payload)]
  diagonal <- payload[-ncol(payload), -ncol(payload)] == payload[-1, -1]
  3 * sum(horizontal & vertical & diagonal)
}

penalty_3 <- function(payload) {
  # pattern similar to finder pattern
  # 40 points per item
  payload_int <- apply(payload, 1, as.integer)
  payload_char <- apply(payload_int, 2, as.character)
  horizontal <- apply(payload_char, 2, paste, collapse = "")
  vertical <- apply(payload_char, 1, paste, collapse = "")
  40 * (sum(grepl("10111010000", c(horizontal, vertical))) +
          sum(grepl("00001011101", c(horizontal, vertical))))
}

penalty_4 <- function(payload) {
  # average darkness
  10 * floor(abs(mean(payload) - 0.5) * 20)
}
