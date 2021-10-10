test_that("galois() and inv_galois() work as expected", {
  expect_identical(
    results <- vapply(seq_len(255) - 1, inv_galois, integer(1)),
    galois_series()
  )
  expect_identical(galois(results), 0:254)
})
