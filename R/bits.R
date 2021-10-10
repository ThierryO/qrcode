#' Create a bits object
#' @param x a logical vector
#' @export
#' @importFrom assertthat assert_that noNA
bits <- function(x) {
  assert_that(is.logical(x), noNA(x))
  class(x) <- c("bits", "logical")
  return(x)
}

#' Print a bits vector
#' @param x the object to print
#' @param ... currently ignored
#' @export
print.bits <- function(x, ...) {
  cat(paste(as.integer(x), collapse = ""))
}

#' Combine bits
#'
#' The result inherits qrcode related arugments from the first element.
#' @param ... the bits to concatenate
#' @export
#' @importFrom assertthat has_attr
c.bits <- function(...) {
  dots <- list(...)

  dots_logical <- vapply(
    dots, FUN.VALUE = vector("list", 1),
    function(i) {
      list(as.logical(i))
    }
  )
  result <- bits(do.call(c, dots_logical))
  if (!has_attr(dots[[1]], "ecword")) {
    return(result)
  }
  attributes(result) <- attributes(dots[[1]])
  return(result)
}

#' Convert a bits object to an integer
#' @param i the bits object
#' @export
#' @importFrom assertthat assert_that
bits2int <- function(i) {
  assert_that(inherits(i, "bits"))
  sum(2 ^ rev(seq_along(i) - 1)[i])
}

#' Convert an integer to a bits object
#' @param i the integer
#' @param n_bit the number of bits
#' @export
#' @importFrom assertthat assert_that is.count is.number
#' @importFrom utils head
int2bits <- function(i, n_bit = 16) {
  assert_that(is.number(i), is.count(n_bit))
  bits(as.logical(rev(head(intToBits(i), n_bit))))
}

#' Convert a bits object into a character string
#' @param x the bits object
#' @param ... currently ignore
#' @export
as.character.bits <- function(x, ...) {
  paste(as.character(as.integer(x)), collapse = "")
}
