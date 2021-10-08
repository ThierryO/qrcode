test_that("qr_code() has a stable output for alphanumeric", {
  test_string <- rep(
    LETTERS, ceiling(max(qrCodeSpec$Alphanumeric) / length(LETTERS))
  )
  for (i in seq_len(6 * 4)) {
    x <- paste(head(test_string, qrCodeSpec$Alphanumeric[i]), collapse = "")
    ecl <- as.character(qrCodeSpec$ECL[i])
    expect_snapshot_value(
      qr_code(x = x, ecl = ecl), cran = TRUE, style = "json2"
    )
  }
})
