#'  QRcode specifications and requirements.
#'
#'  List of different versions of QRcode specification and requirements.
#'  For more details can refer to QRcode standard.
#'
#'  \itemize{
#'    \item Version. QRcode version.
#'    \item ECL. Error Correction Level. Consisted of 4 level L,M,Q and H.
#'    \item Numeric. Number of numeric character supported by the given version and ECL.
#'    \item Alphanumeric. Number of alphabet and numeric character
#'      supported by the given version and ECL.
#'    \item Byte.Number of byte supported by the given version and ECL.
#'    \item Dcword. Data code word count.
#'    \item ECwordPerBlock. Error correction word count per block.
#'    \item Grp1. Number of block in group 1.
#'    \item DCinGrp1. Number of data code word in each group 1.
#'    \item Grp2. Number of block in group 2.
#'    \item DCinGrp2. Number of data code word in each group 2.
#'  }
#'
#' @name qrCodeSpec
#' @docType data
#' @keywords dataset
#' @noRd
"qrCodeSpec"
