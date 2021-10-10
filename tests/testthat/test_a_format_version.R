test_that("format_version() works as expected", {
  expect_identical(as.character(format_version(7)), "000111110010010100")
  expect_identical(as.character(format_version(8)), "001000010110111100")
})
