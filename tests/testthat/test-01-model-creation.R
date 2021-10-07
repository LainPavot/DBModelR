
testthat::test_that("Models creation", {
  testthat::expect_equal(Adress()$.meta__$table_name$name, "Adress")
  testthat::expect_equal(as.character(Adress()$.meta__$table_name), "Adress")
  testthat::expect_equal(as.character(Adress()), paste(
    "AdressModel[",
    "  id<numeric:INTEGER>,",
    "  street<character:TEXT>,",
    "  number<numeric:INTEGER>",
    "]",
    sep="\n"
  ))
  fields <- Adress()$.meta__$fields
  testthat::expect_equal(names(fields), c ("id", "street", "number"))
  testthat::expect_equal(Person()$.meta__$table_name$name, "Person")
  testthat::expect_equal(as.character(Person()$.meta__$table_name), "Person")
  testthat::expect_equal(as.character(Person()), paste(
    "PersonModel[",
    "  name<character:TEXT>,",
    "  fictional<logical:BOOLEAN>,",
    "  height<numeric:FLOAT>,",
    "  picture<blob:BLOB>,",
    "  adress<ANY:OBJECT>,",
    "  birthdate<POSIXct:DATE>",
    "]",
    sep="\n"
  ))
  fields <- Person()$.meta__$fields
  testthat::expect_equal(names(fields), c (
    "name", "fictional", "height", "picture", "adress", "birthdate"
  ))
})
