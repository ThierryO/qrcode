#' Create the message and error code bit string
#'
#' The function returns a bit string containing the message.
#' @inheritParams qr_code
#' @returns The message as a `bits()` object.
#' @examples
#' qr_error("HELLO WORLD")
#' @export
#' @importFrom assertthat assert_that
#' @importFrom stats na.omit
#' @importFrom utils head tail
#' @author Thierry Onkelinx
#' @family internal
qr_error <- function(x, ecl = c("L", "M", "Q", "H")) {
  bit_string <- qr_encode(x = x, ecl = ecl)
  codewords <- attr(bit_string, "n1") * attr(bit_string, "dcword1") +
    attr(bit_string, "n2") * attr(bit_string, "dcword2")
  bit_string <- c(
    bit_string, (rep(FALSE, pmin(8 * codewords - length(bit_string), 4)))
  )
  bit_string <- c(bit_string, rep(FALSE, (8 - length(bit_string) %% 8) %% 8))
  pad_byte <- list(
    bits(c(TRUE, TRUE, TRUE, FALSE, TRUE, TRUE, FALSE, FALSE)),
    bits(c(FALSE, FALSE, FALSE, TRUE, FALSE, FALSE, FALSE, TRUE))
  )
  bit_string <- do.call(
    c,
    c(
      list(bit_string),
      pad_byte[rep_len(1:2, codewords - length(bit_string) / 8)]
    )
  )
  codeword <- matrix(bit_string, nrow = 8)
  message_poly_base <- as.vector(2 ^ (7:0) %*% codeword)
  messages <- matrix(
    NA_integer_, nrow = attr(bit_string, "n1") + attr(bit_string, "n2"),
    ncol = max(attr(bit_string, "dcword1"), attr(bit_string, "dcword2"))
  )
  for (i in seq_len(attr(bit_string, "n1"))) {
    messages[i, seq_len(attr(bit_string, "dcword1"))] <- head(
      message_poly_base, attr(bit_string, "dcword1")
    )
    message_poly_base <- tail(message_poly_base, -attr(bit_string, "dcword1"))
  }
  if (attr(bit_string, "n2") > 0) {
    for (i in attr(bit_string, "n1") + seq_len(attr(bit_string, "n2"))) {
      messages[i, seq_len(attr(bit_string, "dcword2"))] <- head(
        message_poly_base, attr(bit_string, "dcword2")
      )
      message_poly_base <- tail(message_poly_base, -attr(bit_string, "dcword2"))
    }
  }
  assert_that(
    length(message_poly_base) == 0,
    msg = "Unexpected bug in qr_error().
Please contact the maintainer and provide a reproducible example."
  )
  generator_poly <- generator(attr(bit_string, "ecword") - 1)
  error_correction <- apply(
    messages, MARGIN = 1, generator_poly = generator_poly,
    FUN = function(z, generator_poly) {
      divide(message_poly = na.omit(z), generator_poly = generator_poly)
    }
  )
  full <- c(na.omit(as.vector(messages)), as.vector(t(error_correction)))
  full <- bits(as.logical(as.vector(vapply(full, intToBits, raw(32))[8:1, ])))
  full <- c(full, rep(FALSE, attr(bit_string, "remainder")))
  attributes(full) <- attributes(bit_string)
  return(full)
}

generator <- function(b) {
  if (b == 0) {
    return(c(0L, 0L))
  }
  params <- outer(c(0L, b), generator(b - 1L), FUN = "+") %% 255
  new_params <- galois_xor(
    a = inv_galois(tail(params[1, ], -1)),
    b = inv_galois(head(params[2, ], -1))
  )
  c(params[1, 1], galois(new_params), params[2, ncol(params)])
}

galois_xor <- function(a, b) {
  bits1 <- vapply(a, intToBits, raw(32))
  bits2 <- vapply(b, intToBits, raw(32))
  packBits(xor(bits1, bits2), "integer")
}

galois_series <- function() {
  gf <- integer(255)
  gf[1] <- 1L
  for (i in 2:255) {
    gf[i] <- 2L * gf[i - 1]
    if (gf[i] > 255) {
      gf[i] <- packBits(xor(intToBits(gf[i]), intToBits(285)), "integer")
    }
  }
  return(gf)
}

galois <- function(i) {
  vapply(
    i, FUN.VALUE = integer(1), gf = galois_series(),
    FUN = function(z, gf) {
      which(z == gf) - 1L
    }
  )
}

inv_galois <- function(i) {
  galois_series()[i + 1]
}

divide <- function(message_poly, generator_poly) {
  remainder <- c(message_poly, rep(0, length(generator_poly)))
  while (length(remainder) > length(generator_poly)) {
    if (remainder[1] == 0) {
      remainder <- tail(remainder, -1)
      next
    }
    delta <- inv_galois((galois(remainder[1]) + generator_poly) %% 255)
    remainder <- tail(c(
      galois_xor(head(remainder, length(generator_poly)), delta),
      tail(remainder, -length(generator_poly))
    ), -1)
  }
  return(head(remainder, -1))
}
