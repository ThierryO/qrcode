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
qrMask <- function(data, qrInfo, mask) {

  size <- 21 + (qrInfo$Version - 1) * 4
  #mask
  #
  #M0, (row + column) %% 2 == 0
  #M1, (row) %% 2 == 0
  #M2, (column) %% 3 == 0
  #M3, (row + column) %% 3 == 0
  #M4, ( row%/%2 + column%/%3 ) %% 2 == 0
  #M5, ((row * column) %% 2) + ((row * column) %% 3) == 0
  #M6, ( ((row * column) %% 2) + ((row * column) %% 3) ) %% 2 == 0
  #M7, ( ((row + column) %% 2) + ((row * column) %% 3) ) %% 2 == 0
  #
  testString <- c(
    "maskresult<-(row + column) %% 2 == 0", "maskresult<-(row) %% 2 == 0",
    "maskresult<-(column) %% 3 == 0", "maskresult<-(row + column) %% 3 == 0",
    "maskresult<-(row%/%2 + column%/%3) %% 2 == 0",
    "maskresult<-((row * column) %% 2) + ((row * column) %% 3) == 0",
    "maskresult<-( ((row * column) %% 2) + ((row * column) %% 3) ) %% 2 == 0",
    "maskresult<-(((row + column) %% 2) + ((row * column) %% 3) ) %% 2 == 0"
  )
  dataMask <- data
  maskresult <- TRUE
  for (i in seq_len(size)) {
    #row
    row <- i - 1
    for (j in seq_len(size)) {
      #column
      column <- j - 1
      if (dataMask[i, j] < 3) {
        eval(parse(text = testString[mask + 1]))
        if (maskresult) {
          #toggle if ==0
          if (dataMask[i, j] ==  1) {
            dataMask[i, j] <- 0
          } else {
            dataMask[i, j] <- 1
          }
        }
      }
    }
  }
  dataMask[dataMask == 1] <- 99
  dataMask[dataMask == 0] <- 49



  if (qrInfo$ECL == "L") {
    formatString <- paste0(
      "01", str_pad(intToBin(mask), 3, side = "left", pad = "0"), collapse = ""
    )
  } else if (qrInfo$ECL == "M") {
    formatString <- paste0(
      "00", str_pad(intToBin(mask), 3, side = "left", pad = "0"), collapse = ""
    )
  } else if (qrInfo$ECL == "H") {
    formatString <- paste0(
      "10", str_pad(intToBin(mask), 3, side = "left", pad = "0"), collapse = ""
    )
  } else {
    formatString <- paste0(
      "11", str_pad(intToBin(mask), 3, side = "left", pad = "0"), collapse = ""
    )
  }
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
