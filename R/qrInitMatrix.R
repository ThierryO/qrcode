#' Function to initialize QRcode in matrix for different version
#'
#' Create a basic structre of QRcode in matrix format.
#' Each element in QRcode will be marked as different value.
#'
#' @param version version number of the target QRcode
#'


qrInitMatrix <- function(version) {

  size <- 21 + (version - 1) * 4

  #initialize with 0
  data <- matrix(0, size, size)
  markerMat <- matrix(
    c(
      rep(100, 7), 50, 100, rep(50, 5), 100, 50, 100, 50, 100, 100, 100, 50,
      100, 50, 100, 50, 100, 100, 100, 50, 100, 50, 100, 50, 100, 100, 100, 50,
      100, 50, 100, rep(50, 5), 100, 50, rep(100, 7), 50, rep(50, 8)
    ),
    nrow = 8, ncol = 8, byrow = TRUE
  )
  data[1:8, 1:8] <- markerMat
  markerMat <- matrix(
    c(
      rep(50, 8), rep(100, 7), 50, 100, rep(50, 5), 100, 50, 100, 50, 100, 100,
      100, 50, 100, 50, 100, 50, 100, 100, 100, 50, 100, 50, 100, 50, 100, 100,
      100, 50, 100, 50, 100, rep(50, 5), 100, 50, rep(100, 7), 50
    ),
    nrow = 8, ncol = 8, byrow = TRUE
  )
  data[(size - 7):size, 1:8] <- markerMat
  markerMat <- matrix(
    c(
      rep(50, 8), rep(100, 7), 50, 100, rep(50, 5), 100, 50, 100, 50, 100, 100,
      100, 50, 100, 50, 100, 50, 100, 100, 100, 50, 100, 50, 100, 50, 100, 100,
      100, 50, 100, 50, 100, rep(50, 5), 100, 50, rep(100, 7), 50
    ),
    nrow = 8, ncol = 8
  )
  data[1:8, (size - 7):size] <- markerMat
  data[size - 7, 9] <- 95

  #timing code 95/55
  data[seq(9, size - 8, 2), 7] <- 95
  data[7, seq(9, size - 8, 2)] <- 95
  data[seq(10, size - 8, 2), 7] <- 55
  data[7, seq(10, size - 8, 2)] <- 55

  #format information- code20
  data[c(1:6, 8:9, (size - 6):size), 9] <- 20
  data[9, c(1:6, 8, (size - 7):size)] <- 20

  #version information- code40
  if (version >= 7) {
    data[(size - 8):(size - 10), 1:6] <- 40
    data[1:6, (size - 8):(size - 10)] <- 40
  }

  #version > 1, position adjustment marker is required
  #marker is 5x5
  if (version > 1) {
    markerMat <- matrix(
      c(
        rep(100, 5), 100, 50, 50, 50, 100, 100, 50, 100, 50, 100, 100, 50, 50,
        50, 100, rep(100, 5)
      ),
      nrow = 5, ncol = 5, byrow = TRUE
    )
    pMarkerCount <- (version %/% 7) + 1
    index <- size - 6
    gap <- (index - 7) / pMarkerCount
    pMarkerList <- c(7, seq(1, pMarkerCount, 1) * gap + 7)
    lastIndex <- pMarkerList[length(pMarkerList)]

    for (i in seq_along(pMarkerList)) {
      for (j in seq_along(pMarkerList)) {
        if (
          (pMarkerList[i] == 7 & pMarkerList[j] == 7) |
          (pMarkerList[i] == 7 & pMarkerList[j] == lastIndex) |
          (pMarkerList[j] == 7 & pMarkerList[i] == lastIndex)
        ) {
          #ignore fixed marker
        } else {
          if (pMarkerList[i] > 15 | pMarkerList[j] > 15) {
            data[
              (pMarkerList[i] - 2):(pMarkerList[i] + 2),
              (pMarkerList[j] - 2):(pMarkerList[j] + 2)
            ] <- markerMat
          }
        }
      }
    }
  }
  return(data)
}
