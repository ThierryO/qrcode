#' Generate a QR code for a SEPA payment
#' @param iban the IBAN of the beneficiary.
#' @param beneficiary the name of the beneficiary.
#' @param amount the amount to transfer. Must be in EUR.
#' @param unstructured_reference the unstructured reference.
#' The unstructured reference is a string of maximum 140 characters.
#' @param bic the BIC of the beneficiary.
#' @param purpose the purpose of the payment.
#' @param structured_reference the structured reference.
#' @export
#' @importFrom assertthat assert_that is.string noNA is.number
#' @family qr
#' @examples
#' qr_sepa(
#'   iban = "GB33BUKB20201555555555", beneficiary = "John Doe",
#'   amount = 100, unstructured_reference = "Test payment"
#' ) |>
#'   plot()
qr_sepa <- function(
  iban, beneficiary, amount, unstructured_reference = "", bic = "",
  purpose = "", structured_reference = ""
) {
  assert_that(is.string(iban), noNA(iban), nchar(iban) <= 34)
  assert_that(
    is.string(beneficiary), noNA(beneficiary), nchar(beneficiary) <= 11
  )
  assert_that(is.string(bic), noNA(bic), nchar(bic) <= 11)
  assert_that(is.number(amount), noNA(amount), amount > 0, amount < 1e10)
  assert_that(is.string(purpose), noNA(purpose), nchar(purpose) <= 4)
  assert_that(
    is.string(structured_reference), noNA(structured_reference),
    nchar(structured_reference) <= 35
  )
  assert_that(
    is.string(unstructured_reference), noNA(unstructured_reference),
    nchar(unstructured_reference) <= 140
  )
  stopifnot(
    "You can't have both a structured and unstructured reference" =
      nchar(structured_reference) * nchar(unstructured_reference) == 0
  )
  sprintf(
    "BCD\n002\n1\nSCT\n%s\n%s\n%s\nEUR%.2f\n%s\n%s\n%s", bic, beneficiary,
    iban, amount, purpose, structured_reference, unstructured_reference
  ) |>
    qr_code(ecl = "M") -> output
  class(output) <- c("qr_sepa", class(output))
  return(output)
}
