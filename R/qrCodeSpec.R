#'  QRcode specifications and requirements.
#'
#'  List of different versions of QRcode specification and requirements.
#'  For more details can refer to QRcode standard.
#'
#' - `Version`.
#'   QRcode version.
#' - `ECL`.
#'   Error Correction Level. Consisted of 4 level L,M,Q and H.
#' - `Numeric`.
#'   Number of numeric character supported by the given version and ECL.
#' - `Alphanumeric`.
#'   Number of alphabet and numeric character supported by the given version and
#'   ECL.
#' - `Byte`.
#'   Number of byte supported by the given version and ECL.
#' - `Dcword`.
#'   Data code word count.
#' - `ECwordPerBlock`.
#'   Error correction word count per block.
#' - `Grp1`.
#'   Number of block in group 1.
#' - `DCinGrp1`.
#'   Number of data code word in each group 1.
#' - `Grp2`.
#'   Number of block in group 2.
#' - `DCinGrp2`.
#'   Number of data code word in each group 2.
#'
#' @name qrCodeSpec
#' @docType data
#' @keywords dataset
#' @noRd
"qrCodeSpec"
