#' Function to fill up the data bits
#'
#' Fill up the predefined QRcode matrix with the input binary string.
#'
#' @param allBinary all data in binary in character format.
#' @param data matrix data created by \code{\link{qrFillUpMatrix}}
#' @param version version of the QRcode.
#'
#' @return matrix filled up with the data bits
#' @export


qrFillUpMatrix <- function(allBinary, data, version) {
  counter <- 1
  direction <- 1
  byteCount <- length(allBinary) / 8
  size <- 21 + (version - 1) * 4

  pointer <- c(size, size)

  for (j in seq_len(byteCount)) {
    data[pointer[1], pointer[2]] <- as.integer(allBinary[counter])
    counter <- counter + 1
    for (i in 1:8) {
      if (direction == 1) {
        #UP
        if (is_even(pointer[2])) {
          #odd number, right-hand-side
          pointer <- up_odd(data, pointer)
        } else {
          #even number, left-hand-side
          pointer <- up_even(data, pointer)
          direction <- ifelse(
            is.null(attr(pointer, "direction")), 1, attr(pointer, "direction")
          )
        }
      } else {
        #DOWN
        if (is_even(pointer[2])) {
          #odd number, right-hand-side
          pointer <- down_odd(data, pointer)
        } else {
          #even number, left-hand-side
          pointer <- down_even(data, pointer, size)
          direction <- ifelse(
            is.null(attr(pointer, "direction")), 2, attr(pointer, "direction")
          )
        }
      }
      if (i < 8) {
        data[pointer[1], pointer[2]] <- as.integer(allBinary[counter])
        counter <- counter + 1
      }
    }
  }
  return(data)
}

is_even <- function(x) {
  (x %% 2 == 1 && x > 7) || (x %% 2 == 0 && x < 7)
}

up_odd <- function(data, pointer) {
  if (data[pointer[1], pointer[2] - 1] == 0) {
    return(c(pointer[1], pointer[2] - 1))
  }
  if (data[pointer[1] - 1, pointer[2]] == 0) {
    return(c(pointer[1] - 1, pointer[2]))
  }
  if (data[pointer[1], pointer[2] - 1] == 100) {
    return(
      c(
        pointer[1] +
          ifelse(data[pointer[1] - 1, pointer[2]] %in% c(55, 95), -2, 0),
        pointer[2]
      )
    )
  }
  stop("UP-ODD")
}

down_odd <- function(data, pointer) {
  if (data[pointer[1], pointer[2] - 1] == 0) {
    return(c(pointer[1], pointer[2] - 1))
  }
  if (data[pointer[1] + 1, pointer[2]] == 0) {
    return(c(pointer[1] + 1, pointer[2]))
  }
  if (data[pointer[1], pointer[2] - 1] == 100) {
    return(
      c(
        pointer[1] +
          ifelse(data[pointer[1] + 1, pointer[2]] %in% c(55, 95), 2, 1),
        pointer[2]
      )
    )
  }
  stop("DOWN-ODD")
}

up_even <- function(data, pointer) {
  if (pointer[1] == 1) {
    output <- c(pointer[1], pointer[2] - 1)
    attr(output, "direction") <- 2
    return(output)
  }
  if (data[pointer[1] - 1, pointer[2] + 1] == 0) {
    return(c(pointer[1] - 1, pointer[2] + 1))
  }
  if (data[pointer[1] - 1, pointer[2]] == 0) {
    return(c(pointer[1] - 1, pointer[2]))
  }
  if (data[pointer[1] - 1, pointer[2] + 1] == 20) {
    output <- c(
      pointer[1],
      pointer[2] - ifelse(data[pointer[1], pointer[2] - 1] %in% c(55, 95), 2, 1)
    )
    attr(output, "direction") <- 2
    return(output)
  }
  if (data[pointer[1] - 1, pointer[2] + 1] == 100) {
    return(
      c(
        pointer[1] - ifelse(data[pointer[1] - 1, pointer[2]] == 55, 2, 6),
        pointer[2] + ifelse(data[pointer[1] - 1, pointer[2]] == 55, 0, 1)
      )
    )
  }
  if (data[pointer[1] - 1, pointer[2] + 1] %in% c(55, 95)) {
    if (data[pointer[1] - 2, pointer[2] + 1] == 40) {
      output <- c(1, pointer[2] - 2)
      attr(output, "direction") <- 2
      return(output)
    }
    return(c(pointer[1] - 2, pointer[2] + 1))
  }
  stop("UP-EVEN")
}

down_even <- function(data, pointer, size) {
  if (pointer[1] == size) {
    output <- c(
      pointer[1] - ifelse(data[pointer[1], pointer[2] - 1] == 20, 8, 0),
      pointer[2] - 1
    )
    attr(output, "direction") <- 1
    return(output)
  }
  if (data[pointer[1] + 1, pointer[2] + 1] == 0) {
    return(c(pointer[1] + 1, pointer[2] + 1))
  }
  if (data[pointer[1] + 1, pointer[2]] == 0) {
    return(c(pointer[1] + 1, pointer[2]))
  }
  if (data[pointer[1] + 1, pointer[2] + 1] == 100) {
    return(
      c(
        pointer[1] + ifelse(data[pointer[1] + 1, pointer[2]] == 55, 2, 6),
        pointer[2] + ifelse(data[pointer[1] + 1, pointer[2]] == 55, 0, 1)
      )
    )
  }
  if (data[pointer[1] + 1, pointer[2] + 1] %in% c(40, 50)) {
    output <- c(pointer[1], pointer[2] - 1)
    attr(output, "direction") <- 1
    return(output)
  }
  if (data[pointer[1] + 1, pointer[2] + 1] %in% c(55, 95)) {
    return(c(pointer[1] + 2, pointer[2] + 1))
  }
  stop("DOWN-EVEN")
}
