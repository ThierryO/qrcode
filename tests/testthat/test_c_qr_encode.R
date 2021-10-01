test_that("qr_encode() works as expected", {
  expect_type(qr_encode("0"), "logical")
  expect_type(qr_encode("0", "M"), "logical")
  expect_type(qr_encode("0.1"), "logical")
  x <- "fa\xE7ile"
  Encoding(x) <- "latin1"
  expect_type(qr_encode(x), "logical")
})
