#' Function to interleave the Data Code and Error Correction Core
#'
#' @param poly error correction code word polynomial
#' @param dataPoly input data code word polynomial
#' @param qrInfo dataframe that store all the required info to
#' generate QRcode. Via \code{qrVersionInfo}
#'
#' @return Interleaved polynomial readied to fill up the QRcode matrix
#'
#' @export
#' @importFrom R.utils intToBin
#' @author Victor Teh
#' @family legacy

qrInterleave <- function(poly, dataPoly, qrInfo) {
  .Deprecated("qr_code")
  groupCount <- c(qrInfo$Grp1, qrInfo$Grp2)

  counter <- 1
  # grp1Poly
  for (i in seq_len(groupCount[1])) {
    targetDataPoly <- dataPoly[counter:(counter + qrInfo$DCinGrp1 - 1)]
    counter <- counter + qrInfo$DCinGrp1
    if (i == 1) {
      suppressWarnings(classes = "deprecatedWarning", {
        grp1ECPoly <- ECgenerator(
          poly, targetDataPoly, qrInfo$DCinGrp1, qrInfo$ECwordPerBlock
        )
      })
      grp1DCPoly <- targetDataPoly
    } else {
      suppressWarnings(classes = "deprecatedWarning", {
        grp1ECPoly <- rbind(
          grp1ECPoly,
          ECgenerator(
            poly, targetDataPoly, qrInfo$DCinGrp1, qrInfo$ECwordPerBlock
          )
        )
      })
      grp1DCPoly <- rbind(grp1DCPoly, targetDataPoly)
    }
  }

  #grp2Poly
  if (groupCount[2] != 0) {
    grp1ECPoly <- grp1ECPoly
    grp1DCPoly <- cbind(grp1DCPoly, -1)
    for (i in seq_len(groupCount[2])) {
      targetDataPoly <- dataPoly[counter:(counter + qrInfo$DCinGrp2 - 1)]
      counter <- counter + qrInfo$DCinGrp2
      grp1ECPoly <- rbind(
        grp1ECPoly,
        ECgenerator(
          poly, targetDataPoly, qrInfo$DCinGrp2, qrInfo$ECwordPerBlock
        )
      )
      grp1DCPoly <- rbind(grp1DCPoly, targetDataPoly)
    }
  }

  #interleave
  targetECPoly <- grp1ECPoly[seq_along(grp1ECPoly)]
  targetDCPoly <- grp1DCPoly[seq_along(grp1DCPoly)]
  targetECPoly <- targetECPoly[targetECPoly >= 0]
  targetDCPoly <- targetDCPoly[targetDCPoly >= 0]

  ecBin <- paste0(intToBin(targetECPoly), collapse = "")
  dcBin <- paste0(intToBin(targetDCPoly), collapse = "")
  allBinary <- paste0(dcBin, ecBin, collapse = "")
  unlist(strsplit(allBinary, split = ""))
}
