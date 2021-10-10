test_that("qr_version() works as expected", {
  expect_type(z <- qr_version("0"), "list")
  expect_identical(z[1:3], list(version = 1L, ecl = "L", mode = "Numeric"))
  expect_type(z <- qr_version("0", "M"), "list")
  expect_identical(z[1:3], list(version = 1L, ecl = "M", mode = "Numeric"))
  expect_type(z <- qr_version("100"), "list")
  expect_identical(z[1:3], list(version = 1L, ecl = "L", mode = "Numeric"))
  expect_type(z <- qr_version("0.1"), "list")
  expect_identical(z[1:3], list(version = 1L, ecl = "L", mode = "Alphanumeric"))
  expect_type(z <- qr_version("0.1", "M"), "list")
  expect_identical(z[1:3], list(version = 1L, ecl = "M", mode = "Alphanumeric"))
  x <- "fa\xE7ile"
  Encoding(x) <- "latin1"
  expect_type(z <- qr_version(x), "list")
  expect_identical(z[1:3], list(version = 1L, ecl = "L", mode = "Byte"))
  expect_type(
    z <- qr_version(
      "This text requires version 5 with error correction level Q", ecl = "Q"
    ),
    "list"
  )
  expect_identical(z[1:3], list(version = 5L, ecl = "Q", mode = "Byte"))
  set.seed(20211005)
  expect_type(
    z <- qr_version(
      paste(sample(letters, 100, replace = TRUE), collapse = ""), ecl = "Q"
    ),
    "list"
  )
  expect_identical(z[1:3], list(version = 8L, ecl = "Q", mode = "Byte"))
  expect_type(
    z <- qr_version(
      paste(sample(letters, 1600, replace = TRUE), collapse = ""), ecl = "Q"
    ),
    "list"
  )
  expect_identical(z[1:3], list(version = 40L, ecl = "Q", mode = "Byte"))
  expect_true(has_attr(z$bit_string, "version"))
  expect_true(has_attr(z$bit_string, "ecl"))
  expect_true(has_attr(z$bit_string, "dcword1"))
  expect_true(has_attr(z$bit_string, "n1"))
  expect_true(has_attr(z$bit_string, "dcword2"))
  expect_true(has_attr(z$bit_string, "n2"))
  expect_true(has_attr(z$bit_string, "ecword"))
  expect_true(has_attr(z$bit_string, "remainder"))
  expect_true(has_attr(z$bit_string, "alignment"))
})
