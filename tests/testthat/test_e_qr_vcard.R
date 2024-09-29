test_that("qr_vcard()", {
  expect_s3_class(qr_vcard(given = "Jane", family = "Doe"), "qr_vcard")
  expect_s3_class(
    qr_vcard(
      given = "Jane", family = "Doe", middle = "J.", prefix = "Dr.",
      suffix = "PhD", email = "jane@doe.com", tel = "+1234567890",
      url = "https://jane.doe.com", organisation = "Doe Inc.",
      job_title = "CEO", gender = "F", logo = "https://jane.doe.com/logo.png",
      photo = "https://jane.doe.com/jane.jpg",
      address = c(
        street_nr = "123 Main St.", city = "Anytown", region = "NY",
        postal_code = "12345", country = "USA"
      )
    ),
    "qr_vcard"
  )
  expect_s3_class(
    qr_vcard(
      given = "Jane", family = "Doe", middle = c("J.", "J."),
      prefix = c("Dr.", "ir."), suffix = c("PhD", "MSc"),
      email = c(work = "jane@doe.com", "jane@hotmail.com"), tel = "+1234567890",
      url = "https://jane.doe.com", organisation = "Doe Inc.",
      job_title = "CEO", gender = "F", logo = "https://jane.doe.com/logo.png",
      address = list(
        work = c(
          street_nr = "123 Main St.", city = "Anytown", region = "NY",
          postal_code = "12345", country = "USA", pobox = "PO Box 123"
        ),
        home = c(
          street_nr = "321 Main St.", city = "Anytown", region = "NY",
          postal_code = "12345", country = "USA"
        )
      ),
      photo =
"https://jane.doe.com/public/employees/photos/curriculum_vitae/jane_doe.jpg"
    ),
    "qr_vcard"
  )
})
