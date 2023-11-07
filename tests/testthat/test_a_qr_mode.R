test_that("qr_mode() works as expected", {
  expect_identical(qr_mode("0"), "Numeric")
  expect_identical(qr_mode("100"), "Numeric")
  expect_identical(qr_mode("0.1"), "Alphanumeric")
  expect_identical(qr_mode("A"), "Alphanumeric")
  expect_identical(qr_mode("a"), "Byte")
  x <- "fa\xE7ile"
  Encoding(x) <- "latin1"
  expect_identical(qr_mode(x), "Byte")
  expect_identical(qr_mode("\u00E6"), "Byte")
})
