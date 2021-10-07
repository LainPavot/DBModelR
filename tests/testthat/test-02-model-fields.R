
testthat::test_that("Models fields defaults", {
  testthat::expect_equal(Adress()$number, 3)
})

testthat::test_that("Models fields manipulation", {
  adress <- Adress()
  adress$number <- 42
  testthat::expect_equal(adress$number, 42)
  testthat::expect_equal(adress$.raw_number__$get(adress), 42)
})
