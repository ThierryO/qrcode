test_that("qrcode_gen() has a stable output", {
  expect_snapshot_value(
    qrcode_gen("www.r-project.org", dataOutput = TRUE, plotQRcode = FALSE),
    cran = TRUE, style = "json2"
  )
  expect_snapshot_value(
    qrcode_gen(
      "www.r-project.org", dataOutput = TRUE, plotQRcode = FALSE,
      ErrorCorrectionLevel = "M"
    ),
    cran = TRUE, style = "json2"
  )
  expect_snapshot_value(
    qrcode_gen(
      "www.r-project.org", dataOutput = TRUE, plotQRcode = FALSE,
      ErrorCorrectionLevel = "H"
    ),
    cran = TRUE, style = "json2"
  )
  expect_snapshot_value(
    qrcode_gen(
      "www.r-project.org", dataOutput = TRUE, plotQRcode = FALSE,
      ErrorCorrectionLevel = "Q"
    ),
    cran = TRUE, style = "json2"
  )
})
