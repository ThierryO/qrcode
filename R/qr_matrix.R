#' Prepare matrices with default patterns and unmasked data
#' A list with a matrix containing the default patterns (finder pattern,
#' timing pattern, separators, alignment pattern and dark module), the unmask
#' data pattern and the version.
#' @inheritParams qr_code
#' @examples
#' qr_matrix("HELLO WORLD")
#' @export
#' @importFrom assertthat assert_that
#' @importFrom utils head tail
#' @author Thierry Onkelinx
#' @family internal
qr_matrix <- function(x, ecl = c("L", "M", "Q", "H")) {
  payload <- qr_error(x = x, ecl = ecl)
  code_dim <- 4 * (attr(payload, "version") - 1) + 21
  code_matrix <- matrix(NA, nrow = code_dim, ncol = code_dim)

  # finding patterns
  code_matrix[1:7, 1:7] <- TRUE
  code_matrix[2:6, c(2, 6)] <- FALSE
  code_matrix[c(2, 6), 2:6] <- FALSE
  code_matrix[1:7 + code_dim - 7, 1:7] <- TRUE
  code_matrix[2:6 + code_dim - 7, c(2, 6)] <- FALSE
  code_matrix[c(2, 6) + code_dim - 7, 2:6] <- FALSE
  code_matrix[1:7, 1:7 + code_dim - 7] <- TRUE
  code_matrix[2:6, c(2, 6) + code_dim - 7] <- FALSE
  code_matrix[c(2, 6), 2:6 + code_dim - 7] <- FALSE

  # separators
  code_matrix[1:8, c(8, code_dim - 7)] <- FALSE
  code_matrix[code_dim - 0:7, 8] <- FALSE
  code_matrix[c(8, code_dim - 7), 1:8] <- FALSE
  code_matrix[8, code_dim - 0:7] <- FALSE

  # alignment patterns
  if (length(attr(payload, "alignment"))) {
    position <- expand.grid(
      x = attr(payload, "alignment"),
      y = attr(payload, "alignment")
    )
    position <- position[
      position$x != head(attr(payload, "alignment"), 1) |
      position$y != head(attr(payload, "alignment"), 1),
    ]
    position <- position[
      position$x != tail(attr(payload, "alignment"), 1) |
        position$y != head(attr(payload, "alignment"), 1),
    ]
    position <- position[
      position$x != head(attr(payload, "alignment"), 1) |
        position$y != tail(attr(payload, "alignment"), 1),
    ]
    # positive
    pattern <- rbind(
      expand.grid(
        dx = -2:2, dy = c(-2, 2)
      ),
      expand.grid(
        dx = c(-2, 2), dy = -2:2
      ),
      data.frame(dx = 0, dy = 0)
    )
    alignment <- expand.grid(
      position = seq_along(position$x), pattern = seq_along(pattern$dx)
    )
    alignment <- as.matrix(
      position[alignment$position, ] + pattern[alignment$pattern, ]
    )
    code_matrix[alignment] <- TRUE

    # negative
    pattern <- rbind(
      expand.grid(
        dx = -1:1, dy = c(-1, 1)
      ),
      expand.grid(
        dx = c(-1, 1), dy = -1:1
      )
    )
    alignment <- expand.grid(
      position = seq_along(position$x), pattern = seq_along(pattern$dx)
    )
    alignment <- as.matrix(
      position[alignment$position, ] + pattern[alignment$pattern, ]
    )
    code_matrix[alignment] <- FALSE
  }

  # timing patterns
  code_matrix[seq(9, code_dim - 7, by = 2), 7] <- TRUE
  code_matrix[seq(10, code_dim - 7, by = 2), 7] <- FALSE
  code_matrix[7, seq(9, code_dim - 7, by = 2)] <- TRUE
  code_matrix[7, seq(10, code_dim - 7, by = 2)] <- FALSE

  # dark module
  code_matrix[4 * attr(payload, "version") + 10, 9] <- TRUE

  # data areas
  available <- is.na(code_matrix)
  available[1:9, 1:9] <- FALSE
  available[9, code_dim - 0:7] <- FALSE
  available[code_dim - 0:6, 9] <- FALSE
  if (attr(payload, "version") >= 7) {
    available[code_dim - 8:10, 1:6] <- FALSE
    available[1:6, code_dim - 8:10] <- FALSE
  }

  data_matrix <- available
  data_matrix[data_matrix == TRUE] <- NA
  upward <- TRUE
  current_col <- tail(which(colSums(is.na(data_matrix)) > 0), 1)
  i <- 1
  while (length(current_col) > 0) {
    wrow <- which(rowSums(is.na(data_matrix[, current_col - 0:1])) > 0)
    if (length(wrow) == 0) {
      current_col <- tail(which(colSums(is.na(data_matrix)) > 0), 1)
      upward <- !upward
      next
    }
    wrow <- ifelse(upward, tail(wrow, 1), head(wrow, 1))
    if (is.na(data_matrix[wrow, current_col])) {
      data_matrix[wrow, current_col] <- payload[i]
    } else {
      data_matrix[wrow, current_col - 1] <- payload[i]
    }
    i <- i + 1
  }

  list(
    patterns = code_matrix, payload = data_matrix,
    version = attr(payload, "version")
  )
}
