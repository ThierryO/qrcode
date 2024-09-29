#' Create a QR code for a vCard
#' @param given The given name.
#' @param family The family name.
#' @param address In case of a single address, a named character vector with
#' the following elements: `street_nr`, `city`, `region`, `postal_code` and
#' `country`.
#' In case of multiple addresses, a named list of named character
#' vectors.
#' The names of the list are used as the type of the address.
#' @param email Optionally one or more email addresses.
#' The names of the vector are used as the type of the email address.
#' @param telephone Optionally one of more telephone numbers.
#' The names of the vector are used as the type of the telephone number.
#' @param organisation Optionally the name of your organisation and team within
#' the organisation.
#' @param job_title Optionally the job title of the person.
#' @param url Optionally one or more URLs.
#' The names of the vector are used as the type of the URL.
#' @param gender Optionally a string describing the gender of the person.
#' @param logo Optionally a URL to a logo.
#' @param photo Optionally a URL to a photo.
#' @param middle Optionally one or more middle names.
#' @param prefix Optionally one or more prefixes.
#' @param suffix Optionally one or more suffixes.
#' @inheritParams qr_code
#' @param ... Additional arguments are silently ignored.
#' @export
#' @importFrom assertthat assert_that is.string noNA
qr_vcard <- function(
  given, family, address, email, telephone, organisation, job_title, url,
  gender, logo, photo, middle = character(0), prefix = character(0),
  suffix = character(0), ecl = c("L", "M", "Q", "H"), ...
) {
  assert_that(
    is.string(given), is.string(family), noNA(given), noNA(family),
    is.character(middle), is.character(prefix), is.character(suffix),
    noNA(middle), noNA(prefix), noNA(suffix)
  )
  ecl <- match.arg(ecl)
  given <- vcard_escape(given)
  family <- vcard_escape(family)
  middle <- vcard_escape(middle)
  prefix <- vcard_escape(prefix)
  suffix <- vcard_escape(suffix)
  c(
    "BEGIN:VCARD", "VERSION:4.0",
    vcard_fname(
      given = given, family = family, middle = middle, prefix = prefix,
      suffix = suffix
    ),
    vcard_name(
      given = given, family = family, middle = middle, prefix = prefix,
      suffix = suffix
    ),
    vcard_address(address), vcard_multi_type(x = email, element = "EMAIL"),
    vcard_multi_type(x = telephone, element = "TEL"),
    vcard_multi(x = organisation, element = "ORG"),
    vcard_multi(x = job_title, element = "TITLE"),
    vcard_multi_type(x = url, element = "URL"),
    vcard_multi_type(x = logo, element = "LOGO"),
    vcard_multi_type(x = photo, element = "PHOTO"),
    vcard_single(x = gender, element = "GENDER"), "END:VCARD"
  ) |>
    vapply(vcard_wrap, character(1), width = 75) |>
    paste(collapse = "\r\n") |>
    qr_code(ecl = ecl) -> output
  class(output) <- c("qr_vcard", class(output))
  return(output)
}

#' @importFrom assertthat assert_that is.count is.string noNA
vcard_wrap <- function(x, width = 75) {
  assert_that(is.string(x), noNA(x), is.count(width))
  if (nchar(x, type = "bytes") < width) {
    return(x)
  }
  parts <- strsplit(x, split = "")[[1]]
  output <- character(0)
  while (length(parts) > 0) {
    n_bytes <- nchar(parts, type = "bytes")
    n_total <- cumsum(n_bytes) + seq_along(n_bytes) - 1
    output |>
      c(paste(parts[which(n_total <= width)], collapse = "")) -> output
    parts <- parts[-which(n_total <= width)]
  }
  paste(output, collapse = "\r\n ")
}

vcard_fname <- function(
  given, family, middle = character(0), prefix = character(0),
  suffix = character(0)
) {
  if (length(prefix) > 1) {
    prefix <- paste(prefix, collapse = " ")
  }
  if (length(middle) > 1) {
    middle <- paste(middle, collapse = " ")
  }
  if (length(suffix) > 1) {
    suffix <- paste(suffix, collapse = "\\,")
  }
  c(prefix, given, middle, family, suffix) |>
    paste(collapse = " ") |>
    sprintf(fmt = "FN:%s")
}

vcard_name <- function(
  given, family, middle = character(0), prefix = character(0),
  suffix = character(0)
) {
  if (length(prefix) > 1) {
    prefix <- paste(prefix, collapse = "\\,")
  }
  if (length(middle) > 1) {
    middle <- paste(middle, collapse = "\\,")
  }
  if (length(suffix) > 1) {
    suffix <- paste(suffix, collapse = "\\,")
  }
  c(family, given, middle, prefix, suffix) |>
    paste(collapse = ";") |>
    sprintf(fmt = "N:%s")
}

vcard_escape <- function(x) {
  stopifnot(
    "vCard requires UTF-8 strings" = Encoding(x) %in% c("unknown", "UTF-8")
  )
  gsub(pattern = "\\\\", replacement = "\\\\\\\\", x = x) |>
    gsub(pattern = ",",  replacement = "\\,", x = _) |>
    gsub(pattern = ";", replacement = "\\;", x = _) |>
    gsub(pattern = "\n", replacement = "\\n", x = _)
}

#' @importFrom assertthat assert_that is.string noNA
vcard_address <- function(address, type = "") {
  if (missing(address)) {
    return(character(0))
  }
  stopifnot("address must be named" = !is.null(names(address)))
  assert_that(is.string(type), noNA(type))
  if (is.list(address)) {
    vapply(
      names(address), FUN.VALUE = character(1), address = address,
      FUN = function(i, address) {
        vcard_address(address[[i]], type = i)
      }
    ) -> output
    return(output)
  }
  relevant <- c("street_nr", "city", "region", "postal_code", "country")
  if (any(!names(address) %in% relevant)) {
    paste0("`", relevant, "`", collapse = ", ") |>
      sprintf(
        fmt = "address only considers element with the following names: %s"
      ) |>
      warning()
  }
  address <- address[relevant]
  address[is.na(address)] <- ""
  vcard_escape(address) |>
    paste(collapse = ";") |>
    sprintf(
      fmt = "ADR%2$s:;;%1$s", ifelse(type != "", paste0(";Type=", type), "")
    )
}

#' @importFrom assertthat assert_that is.string noNA
vcard_multi <- function(x, element) {
  if (missing(x)) {
    return(character(0))
  }
  assert_that(is.character(x), noNA(x), is.string(element), noNA(element))
  vcard_escape(x) |>
    paste(collapse = ";") |>
    sprintf(fmt = "%2$s:%1$s", element)
}

#' @importFrom assertthat assert_that is.string noNA
vcard_multi_type <- function(x, element) {
  if (missing(x)) {
    return(character(0))
  }
  assert_that(is.character(x), noNA(x), is.string(element), noNA(element))
  if (is.null(names(x))) {
    names(x) <- NA
  }
  vcard_escape(x) |>
    sprintf(
      fmt = "%2$s%3$s:%1$s", element,
      ifelse(!is.na(names(x)), paste0(";Type=", names(x)), "")
    )
}

#' @importFrom assertthat assert_that is.string noNA
vcard_single <- function(x, element) {
  if (missing(x)) {
    return(character(0))
  }
  assert_that(is.string(x), noNA(x), is.string(element), noNA(element))
  vcard_escape(x) |>
    sprintf(fmt = "%2$s:%1$s", element)
}
