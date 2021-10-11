#' Function to convert input data string to binary polynomial
#'
#' @description  Convert input data string to binary polynomial
#'
#' @param dataString input data string.
#' @param qrInfo dataframe that store all the required info to generate qrcode.
#'
#'
#' @importFrom R.utils intToBin
#' @importFrom stringr str_pad
#' @export
#' @author Victor Teh
#' @family legacy
#'
DataStringBinary <- function(dataString, qrInfo) { #nolint
  .Deprecated("qr_code")

  if (qrInfo$mode == "0100") {
    # ##
    # #byte
     tempBin <- intToBin(utf8ToInt(dataString))
     tempBin <- unlist(
       lapply(tempBin, str_pad, width = 8, side = "left", pad = "0")
      )
     tempBin <- paste(tempBin, collapse = "")
  } else if (qrInfo$mode == "0010") {
    map <- c(0:9, LETTERS, " ", "$", "%", "*", "+", "-", ".", "/", ":")
    key <- seq_along(map) - 1
    alpanumericTable <- data.frame(key, map)

    dataStringTemp <- unlist(strsplit(dataString, split = ""))
    dataStringValue <- sapply(
      dataStringTemp,
      function(x) {
        alpanumericTable[map == x, 1]
      }
    )
    index <- seq(1, nchar(dataString), 2)

    if (nchar(dataString) %% 2 == 0) {
      tempBin <- sapply(
        index,
        function(x) {
          str_pad(
            intToBin(dataStringValue[x] * 45 + dataStringValue[x + 1]),
            11, side = "left", pad = "0"
          )
        }
      )
    } else {
      tempBin <- c(
        sapply(
          index[1:(length(index) - 1)],
          function(x) {
            str_pad(
              intToBin(dataStringValue[x] * 45 + dataStringValue[x + 1]),
              11, side = "left", pad = "0"
            )
          }
        ),
        str_pad(
          intToBin(dataStringValue[index[length(index)]]), 6, side = "left",
          pad = "0"
        )
      )
    }
    tempBin <- paste(tempBin, collapse = "")
  }

  charCount <- 0
  if (qrInfo$Version <= 9) {
    charCount <- ifelse(
      qrInfo$mode == "0001", 10, ifelse(qrInfo$mode == "0010", 9, 8)
    )
  } else if (qrInfo$Version >= 27) {
    charCount <- ifelse(
      qrInfo$mode == "0001", 14, ifelse(qrInfo$mode == "0010", 13, 16)
    )
  } else {
    charCount <- ifelse(
      qrInfo$mode == "0001", 12, ifelse(qrInfo$mode == "0010", 11, 16)
    )
  }

  #mode and length
  tempBin <- paste0(
    qrInfo$mode,
    str_pad(intToBin(nchar(dataString)), charCount, side = "left", pad = "0"),
    tempBin, collapse = ""
  )
  #pad 0
  if (qrInfo$Dcword * 8 - nchar(tempBin) > 4) {
    tempBin <- paste0(tempBin, paste(rep("0", 4), collapse = ""))
  } else {
    tempBin <- paste0(
      tempBin, paste(
        rep("0", qrInfo$Dcword * 8 - nchar(tempBin)), collapse = ""
      )
    )
  }
  padCount <- 8 - nchar(tempBin) %% 8
  tempBin <- paste0(tempBin, paste(rep("0", padCount), collapse = ""))

  padByte <- c("11101100", "00010001")
  byteCount <- (qrInfo$Dcword * 8 - nchar(tempBin)) / 8
  if (byteCount > 0) {
    if (byteCount == 1) {
      bytearray <- suppressWarnings(cbind(1, padByte[1]))
      bytearray <- paste(bytearray[, 2], collapse = "")
    } else {
      bytearray <- suppressWarnings(cbind(c(1:byteCount), padByte))
      bytearray <- paste(bytearray[, 2], collapse = "")
    }
    tempBin <- paste0(tempBin, bytearray, collapse = "")
  }
  index <- seq(1, nchar(tempBin), 8)
  dataPoly <- sapply(
    index,
    function(x) {
      strtoi(substr(tempBin, x, x + 7), base = 2)
    }
  )
  return(dataPoly)
}
