test_that("qr_code() has a stable output", {
  expect_snapshot_value(
    qr_code("HELLO WORLD"),
    cran = TRUE, style = "json2"
  )
  expect_snapshot_value(
    qr_code("QRCODE"),
    cran = TRUE, style = "json2"
  )
  expect_snapshot_value(
    qr_code("https://www.r-project.org"),
    cran = TRUE, style = "json2"
  )
})
