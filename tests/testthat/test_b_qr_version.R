test_that("qr_version() works as expected", {
  expect_type(z <- qr_version("0"), "list")
  expect_identical(z[1:3], list(version = 1L, ecl = "L", mode = "Numeric"))
  expect_type(z <- qr_version("0", "M"), "list")
  expect_identical(z[1:3], list(version = 1L, ecl = "M", mode = "Numeric"))
  expect_type(z <- qr_version("0.1"), "list")
  expect_identical(z[1:3], list(version = 1L, ecl = "L", mode = "Alphanumeric"))
  expect_type(z <- qr_version("0.1", "M"), "list")
  expect_identical(z[1:3], list(version = 1L, ecl = "M", mode = "Alphanumeric"))
  x <- "fa\xE7ile"
  Encoding(x) <- "latin1"
  expect_type(z <- qr_version(x), "list")
  expect_identical(z[1:3], list(version = 1L, ecl = "L", mode = "Byte"))
})
