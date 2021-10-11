#'  QRcode generator
#'
#' @description Create QRcode in R.
#' Capable to generate all variant of QRcode, version 1 to 40 and Error correct
#' level of "L","M","Q" and " H".
#' Not all reader in market can support all QRcode version,
#' `qrcode_gen()` has a software limit to version 10 which is tested working in
#' most reader.
#'
#'
#' @param dataString input string for the QRcode.
#' @param ErrorCorrectionLevel Error Correction Level.
#' The available options are `"L"`, `"M"`, `"Q"` and `"H"`.
#' Default value as `"L"`.
#' @param dataOutput option to export data as matrix.
#' Default value is `FALSE`.
#' @param plotQRcode option to plot QRcode.
#' Default value is `TRUE`.
#' @param wColor color of the white module(white squre) in QRcode.
#' Default value `"white"`.
#' @param bColor color of the black module(black squre) in QRcode.
#' Default value `"black"`.
#' @param mask mask for QRcode to increase decodability.
#' Available values are `0` to `7`.
#' @param softLimitFlag flag to limit the QRcode version to 10.
#' Default value `TRUE`.
#' @return A matrix that represent the QRcode.
#' 1 as black module and 0 as white module.
#'
#'
#' @examples
#'  qrcode_gen("www.r-project.org")
#'
#'  #User may change the color of the module
#'  qrcode_gen("www.r-project.org", bColor = "Green3")
#'
#' @importFrom stats heatmap
#'
#' @export
#' @author Victor Teh
#' @family legacy

qrcode_gen <- function(
  dataString, ErrorCorrectionLevel = "L", dataOutput = FALSE, plotQRcode = TRUE, #nolint
  wColor = "White", bColor = "black", mask = 1, softLimitFlag = TRUE
) {
  .Deprecated(
    "qr_code",
    msg = "`qrcode_gen()` has known bugs. Please use `qr_code()` instead."
  )
  suppressWarnings(classes = "deprecatedWarning", {
    qrInfo <- qrVersionInfo(dataString, ECLevel = ErrorCorrectionLevel)
  })
  if (softLimitFlag && qrInfo$Version > 10) {
    stop("Input string size too big.
Try lower Error Correction Level or shortern the input string.")
  }
  #initialize a QRcode in a matrix
  suppressWarnings(classes = "deprecatedWarning", {
    data <- qrInitMatrix(qrInfo$Version)
  })

  #convert data string into Binary in polynomial
  suppressWarnings(classes = "deprecatedWarning", {
    dataPoly <- DataStringBinary(dataString, qrInfo)
  })

  #polynomial generator
  suppressWarnings(classes = "deprecatedWarning", {
    poly <- polynomialGenerator(qrInfo$ECwordPerBlock)
  })

  #compile the final binary string.
  suppressWarnings(classes = "deprecatedWarning", {
    allBinary <- qrInterleave(poly, dataPoly, qrInfo)
  })

  #fill up all binary into the QRcode template
  suppressWarnings(classes = "deprecatedWarning", {
    data <- qrFillUpMatrix(allBinary, data, qrInfo$Version)
  })

  #apply mask
  suppressWarnings(classes = "deprecatedWarning", {
    dataMasked <- qrMask(data, qrInfo, mask)
  })
  if (plotQRcode) {
    heatmap(
      dataMasked[rev(seq_len(nrow(dataMasked))), ], Rowv = NA, Colv = NA,
      scale = "none", col = c(wColor, bColor), labRow = "", labCol = ""
    )
  }
  if (dataOutput) {
    return(dataMasked)
  }
}
