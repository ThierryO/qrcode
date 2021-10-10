test_that("qr_code() has a stable output for numeric", {
  skip_if_not(Sys.getenv("DETAILED") != "", "Skipping detailed checks.")
  skip_on_cran()
  skip_on_covr()
  test_string <- c(
    "0", "1", "12", "123", "1234", "12345", "123456", "1230", "12300", "123000"
  )
  for (i in seq_len(length(test_string) * 4) - 1) {
    x <- test_string[i %/% 4 + 1]
    ecl <- c("L", "M", "Q", "H")[i %% 4 + 1]
    expect_true(validate_qr(x = x, ecl = ecl))
  }
})

test_that("qr_code() has a stable output for alphanumeric", {
  skip_if_not(Sys.getenv("DETAILED") != "", "Skipping detailed checks.")
  skip_on_cran()
  skip_on_covr()
  test_string <- c(LETTERS, 0:9, " ", "$", "%", "*", "+", "-", ".", "/", ":")
  test_string <- rep(
    test_string, ceiling(max(qrCodeSpec$Alphanumeric) / length(test_string))
  )
  for (i in seq_along(qrCodeSpec$Alphanumeric)) {
    x <- paste(head(test_string, qrCodeSpec$Alphanumeric[i]), collapse = "")
    ecl <- as.character(qrCodeSpec$ECL[i])
    expect_true(validate_qr(x = x, ecl = ecl))
  }
})
