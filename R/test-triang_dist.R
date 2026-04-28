test_that("dtriang funciona correctamente", {
  expect_equal(dtriang(0.5, 0, 1, 0.5), 2)
  expect_error(dtriang(0.5, 1, 0, 0.5)) # Prueba de error [cite: 50]
})

test_that("ptriang y qtriang son consistentes", {
  p <- 0.5
  q <- qtriang(p, 0, 10, 5)
  expect_equal(ptriang(q, 0, 10, 5), p)
})
