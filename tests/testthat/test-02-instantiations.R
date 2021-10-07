
testthat::test_that("ORM instanciation", {
  orm <- ORM()
  testthat::expect_equal(orm$connection_parameters$host, "localhost")
  testthat::expect_equal(orm$connection_parameters$port, -1)
  testthat::expect_equal(orm$connection_parameters$user, "")
  testthat::expect_equal(orm$connection_parameters$password, "")
  testthat::expect_equal(orm$connection_parameters$database, "")
  testthat::expect_equal(orm$connection_parameters$DBMS, NO_DBMS)
})
