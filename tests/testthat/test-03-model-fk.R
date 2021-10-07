
testthat::test_that("Models fk assignation", {
  test <- builder(
    "test",
    adress=ForeignKeyField(
      Adress(),
      list(this="id", other="id"),
      MANY_TO_MANY
    )
  )()
  testthat::expect_equal(test$adress, NULL)
  testthat::expect_error({
    test$adress <- 4
  }, paste0(
    "ForeignKeyField\\$adress expected a Adress object, ",
    "got a numeric instead."
  ))
  test$adress <- Adress()
  testthat::expect_equal(test$adress, Adress())
  testthat::expect_equal(test$.raw_adress__$get(Adress()), Adress())
})
