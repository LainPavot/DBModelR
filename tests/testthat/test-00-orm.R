
testthat::context("ORM tests")

testthat::test_that("ORM base attributes", {
  orm <- ORM()
  testthat::expect_equal(orm$DBMS, NO_DBMS)
  testthat::expect_equal(orm$connected, FALSE)
  testthat::expect_equal(orm$has_dbms, FALSE)
})

testthat::test_that("ORM DBMS settings", {
  orm <- ORM()
  orm$set_dbms(SQLITE)
  testthat::expect_equal(orm$DBMS, SQLITE)
  testthat::expect_equal(orm$connected, FALSE)
  testthat::expect_equal(orm$has_dbms, TRUE)
})

testthat::test_that("ORM connection", {
  orm <- ORM()
  orm$set_params(ConnectionParams(
    DBMS=SQLITE,
    database="./test.sqlite"
  ))
  testthat::expect_equal(orm$DBMS, SQLITE)
  testthat::expect_equal(orm$connected, FALSE)
  testthat::expect_equal(orm$has_dbms, TRUE)
  orm$connect()
  testthat::expect_equal(orm$connected, TRUE)
  orm$disconnect()
  file.remove("./test.sqlite")
})

testthat::test_that("ORM connection parameters", {
  orm <- ORM()
  orm$set_param("host", "127.0.0.1")
  testthat::expect_equal(orm$host, "127.0.0.1")
  orm$set_param("port", 42)
  testthat::expect_equal(orm$port, 42)
  orm$set_param("database", "test-db")
  testthat::expect_equal(orm$database, "test-db")
  orm$set_param("user", "testUser")
  testthat::expect_equal(orm$user, "testUser")
  orm$set_param("flags", 12345)
  testthat::expect_equal(orm$flags, 12345)
  testthat::expect_equal(
    as.list(orm$connection_parameters),
    list(
      client.flag=12345,
      dbname="test-db",
      username="testUser",
      password="",
      host="127.0.0.1",
      port=42
    )
  )
})