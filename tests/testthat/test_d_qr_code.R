test_that("qr_code() has a stable output for numeric", {
  skip_on_cran()
  test_string <- c(
    "0", "1", "12", "123", "1234", "12345", "123456", "1230", "12300", "123000"
  )
  for (i in seq_len(length(test_string) * 4) - 1) {
    x <- test_string[i %/% 4 + 1]
    ecl <- c("L", "M", "Q", "H")[i %% 4 + 1]
    expect_snapshot_value(
      qr_code(x = x, ecl = ecl), cran = TRUE, style = "json2"
    )
  }
})

test_that("qr_code() has a stable output for alphanumeric", {
  skip_on_cran()
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
