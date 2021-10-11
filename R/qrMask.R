#' Apply mask to the QRcode matrix
#'
#' @param data QRcode matrix
#' @param qrInfo dataframe that store all the required info to generate QRcode.
#' Via [qrVersionInfo()]
#' @param mask mask for QRcode to increase decodability.
#' Available value are `0` to `7`.
#'
#' @details
#'    QRcode stardard specify 8 masks as listed below.
#'-  M0, (row + column) %% 2 == 0
#'-  M1, (row) %% 2 == 0
#'-  M2, (column) %% 3 == 0
#'-  M3, (row + column) %% 3 == 0
#'-  M4, ( row%/%2 + column%/%3 ) %% 2 == 0
#'-  M5, ((row * column) %% 2) + ((row * column) %% 3) == 0
#'-  M6, ( ((row * column) %% 2) + ((row * column) %% 3) ) %% 2 == 0
#'-  M7, ( ((row + column) %% 2) + ((row * column) %% 3) ) %% 2 == 0
#' @export
#' @importFrom R.utils intToBin
#' @importFrom stringr str_pad
#' @author Victor Teh
#' @family legacy
qrMask <- function(data, qrInfo, mask) {
  .Deprecated("qr_code")

  size <- 21 + (qrInfo$Version - 1) * 4

  maskresult <- function(mask) {
    switch(
      as.character(mask),
      "0" = function(row, column) {
        (row + column) %% 2 == 0
      },
      "1" = function(row, column) {
        row %% 2 == 0
      },
      "2" = function(row, column) {
        column %% 3 == 0
      },
      "3" = function(row, column) {
        (row + column) %% 3 == 0
      },
      "4" = function(row, column) {
        (row %/% 2 + column %/% 3) %% 2 == 0
      },
      "5" = function(row, column) {
        ((row * column) %% 2) + ((row * column) %% 3) == 0
      },
      "6" = function(row, column) {
        (((row * column) %% 2) + ((row * column) %% 3)) %% 2 == 0
      },
      "7" = function(row, column) {
        (((row + column) %% 2) + ((row * column) %% 3)) %% 2 == 0
      }
    )
  }
  mask_fun <- maskresult(mask)
  dataMask <- data
  to_mask <- which(dataMask < 3, arr.ind = TRUE)
  to_toggle <- mask_fun(to_mask[, "row"] - 1, to_mask[, "col"] - 1)
  dataMask[to_mask[to_toggle, ]] <- as.integer(!dataMask[to_mask[to_toggle, ]])
  dataMask[dataMask == 1] <- 99
  dataMask[dataMask == 0] <- 49

  formatString <- paste0(
    switch(as.character(qrInfo$ECL), L = "01", M = "00", H = "10", "11"),
    str_pad(intToBin(mask), 3, side = "left", pad = "0"), collapse = ""
  )
  polyString <- "10100110111"

  formatBin <- formatPolyGen(formatString, polyString)
  dataMask[c(1:6, 8:9, (size - 6):size), 9] <- formatBin[15:1]
  dataMask[9, c(1:6, 8, (size - 7):size)] <- formatBin[1:15]

  #version
  if (qrInfo$Version >= 7) {
    versionString <- "000111"#qrInfo$Version
    polyString <- "1111100100101"
    versionBin <- versionPolyGen(versionString, polyString)
    dataMask[(size - 8):(size - 10), 6:1] <- versionBin[18:1]
    dataMask[1:6, (size - 10):(size - 8)] <- matrix(
      versionBin, nrow = 6, ncol = 3, byrow = TRUE
    )
  }

  dataMask[dataMask == 1] <- 98
  dataMask[dataMask == 0] <- 48
  dataMask[dataMask < 60] <- 0
  dataMask[dataMask > 60] <- 1
  return(dataMask)
}
