test_that("qr_event()", {
  expect_s3_class(
    qr_event(start = Sys.time(), end = Sys.time(), title = "my event"),
    c("qr_code", "qr_event")
  )
})
