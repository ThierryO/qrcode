test_that("generate_svg()", {
  test_file <- tempfile("generate", fileext = ".svg")
  on.exit(file.remove(test_file), add = TRUE)
  qrc <- qr_code("generate_svg()")
  expect_invisible(generate_svg(qrc, filename = test_file, show = FALSE))
  file_test("-f", test_file)

  test_file2 <- tempfile("generate", fileext = ".svg")
  on.exit(file.remove(test_file2), add = TRUE)
  qrc <- qr_wifi(ssid = "mynetwork", key = "topsecret")
  expect_invisible(generate_svg(qrc, filename = test_file2, show = FALSE))
  file_test("-f", test_file2)
})
