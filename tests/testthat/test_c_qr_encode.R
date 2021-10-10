test_that("qr_encode_numeric() works as expected", {
  expect_identical(as.character(qr_encode_numeric("9")), "1001")
  expect_identical(as.character(qr_encode_numeric("867")), "1101100011")
  expect_identical(as.character(qr_encode_numeric("530")), "1000010010")
})

test_that("qr_encode() works as expected", {
  expect_type(qr_encode("0"), "logical")
  expect_type(qr_encode("0", "M"), "logical")
  expect_type(qr_encode("0.1"), "logical")
  x <- "fa\xE7ile"
  Encoding(x) <- "latin1"
  expect_type(qr_encode(x), "logical")
})
