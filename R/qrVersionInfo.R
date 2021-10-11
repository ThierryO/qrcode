#' Function to identify the version of the QRcode based on input string
#'
#' @param dataString dataString is the input string.
#' @param ECLevel Error Correction Level.
#' In QRcode standard, the are 4 levels "L", "M", "Q" and "H" which represent
#' 7%, 15%, 20% and 30% data recovery capability.
#'
#' @return 1 row dataframe that include all required info to generate QRcode.
#' @importFrom utils data head
#' @export
#' @author Victor Teh
#' @family legacy

qrVersionInfo <- function(dataString, ECLevel = c("L", "M", "Q", "H")) { #nolint
  .Deprecated("qr_version")
  ECLevel <- match.arg(ECLevel) #nolint
  # identify whether the data string is belongs to which category:
  # Alphnumeric or Byte
  if (length(grep("[a-z!?><;@#&()]", dataString)) == 0) {
    mode <- "0010" # Alphanumeric
    qrInfo <- head(
      qrCodeSpec[
        qrCodeSpec$ECL == ECLevel &
          qrCodeSpec$Alphanumeric >= nchar(dataString),
        c(1:2, 4, 6:11)
      ],
      1
    )
  } else {
    mode <- "0100" #Byte
    qrInfo <- head(
      qrCodeSpec[
        qrCodeSpec$ECL == ECLevel & qrCodeSpec$Byte >= nchar(dataString),
        c(1:2, 5:11)
      ],
      1
    )
  }
  qrInfo$mode <- mode

  return(qrInfo)
}
