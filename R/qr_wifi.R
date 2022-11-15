#' Generate QR code with wifi login information
#' @param ssid The SSID of the network.
#' @param encryption The encryption standard.
#' Options are `"WPA"`, `"WEP"` and `""`.
#' The latter implies no encryption.
#' Defaults to `"WPA"`.
#' @param key The key for the encryption.
#' @param hidden Use `FALSE` for a visible SSID.
#' Use `TRUE` for a hidden SSID.
#' Defaults to  `FALSE`.
#' @inheritParams qr_code
#' @export
#' @family qr
qr_wifi <- function(
    ssid, encryption = c("WPA", "WEP", ""), key = "", hidden = FALSE,
    ecl = c("L", "M", "Q", "H")
) {
  encryption <- match.arg(encryption)
  ecl <- match.arg(ecl)
  if (encryption == "") {
    x <- sprintf(
      "WIFI:T:nopass;S:%s;%s", ssid, ifelse(hidden, "", "H;")
    )
  } else {
    x <- sprintf(
      "WIFI:T:%s;S:%s;P:%s;%s", encryption, ssid, key, ifelse(hidden, "", "H;")
    )
  }
  z <- qr_code(x = x, ecl = ecl)
  attr(z, "ssid") <- ssid
  attr(z, "encryption") <- encryption
  attr(z, "hidden") <- hidden
  attr(z, "key") <- key
  class(z) <- c("qr_wifi", class(z))
  return(z)
}
