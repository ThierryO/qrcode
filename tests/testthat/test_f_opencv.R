# use opencv to test QR codes
test_that("test generated codes", {
  skip_if_not_installed("opencv")
  requireNamespace("opencv")
  skip_if(
    identical(Sys.info()[["sysname"]], "Linux") &&
      opencv::ocv_version() < "4.5.2"
  )

  test_read_qr <- function(input) {
    requireNamespace("opencv")
    test_file <- tempfile("qrcode", fileext = ".png")
    png(test_file)
    plot(qr_code(input))
    dev.off()
    opencv::ocv_read(test_file) |>
      opencv::ocv_qr_detect() |>
      expect_equal(input)
    rm(test_file)
  }

  test_read_qr("ABCD")
  test_read_qr("123 abc ABC_")
})
