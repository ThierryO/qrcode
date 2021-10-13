#' Create a bits object
#'
#' Converts a logical vector into a bits object.
#' This remains a logical vector.
#' The main difference is that is printed as a `0` and `1` bit string rather
#' than a `FALSE` and `TRUE` vector
#' @param x a logical vector
#' @examples
#' z <- bits(c(FALSE, TRUE))
#' z
#' str(z)
#' @export
#' @importFrom assertthat assert_that noNA
#' @author Thierry Onkelinx
#' @family bits
bits <- function(x) {
  assert_that(is.logical(x), noNA(x))
  class(x) <- c("bits", "logical")
  return(x)
}

#' Print a bits vector
#' Display the logical vector as a bit string where `FALSE` is shown as `0` and
#' `TRUE` as `1`.
#' @param x the object to print
#' @param ... currently ignored
#' @examples
#' z <- bits(c(FALSE, TRUE))
#' print(z)
#' @export
#' @author Thierry Onkelinx
#' @family bits
print.bits <- function(x, ...) {
  cat(paste(as.integer(x), collapse = ""))
}

#' Combine bits
#'
#' The result inherits arguments from the first element.
#' @param ... the bits to concatenate
#' @examples
#' z <- bits(c(FALSE, TRUE))
#' z
#' c(z, z, rev(z))
#' @export
#' @importFrom assertthat has_attr
#' @author Thierry Onkelinx
#' @family bits
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

#' Convert a bits object to an integer and vice versa
#' @param x the bits object
#' @param i the integer
#' @param n_bit the number of bits
#' @rdname bits2int
#' @export
#' @examples
#' z <- bits(c(FALSE, TRUE, TRUE, FALSE))
#' z
#' y <- bits2int(z)
#' y
#' int2bits(y)
#' int2bits(y, 4)
#' @importFrom assertthat assert_that
#' @author Thierry Onkelinx
#' @family bits
bits2int <- function(x) {
  assert_that(inherits(x, "bits"))
  sum(2 ^ rev(seq_along(x) - 1)[x])
}

#' @rdname bits2int
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
#' @examples
#' z <- bits(c(FALSE, TRUE, TRUE, FALSE))
#' z
#' as.character(z)
#' @export
#' @author Thierry Onkelinx
#' @family bits
as.character.bits <- function(x, ...) {
  paste(as.character(as.integer(x)), collapse = "")
}
