test_that("format_string() works as expected", {
  expect_identical(as.character(format_string("L", 7)), "110100101110110")
  expect_identical(as.character(format_string("M", 0)), "101010000010010")
  expect_identical(as.character(format_string("Q", 3)), "011101000000110")
  expect_identical(as.character(format_string("H", 2)), "001110011100111")
})
