
testthat::context("ORM tests")


if (file.exists(DB_PATH)) {
    file.remove(DB_PATH)
}

orm <- ORM(DB_PATH, MODELS, connect=FALSE)

testthat::test_that("ORM connection tests", {

    testthat::expect_false(file.exists(DB_PATH))
    testthat::expect_false(orm$is_connected())

    testthat::expect_true(orm$connect())
    testthat::expect_true(file.exists(DB_PATH))
    testthat::expect_true(orm$is_connected())
    testthat::expect_true(orm$disconnect(remove=TRUE))
    testthat::expect_false(file.exists(DB_PATH))

    testthat::expect_true(orm$connect())
    testthat::expect_true(file.exists(DB_PATH))
    testthat::expect_true(orm$is_connected())
    testthat::expect_true(orm$disconnect())
    testthat::expect_true(file.exists(DB_PATH))
})

orm$connect()

requests <- do.call(c, purrr::map(orm$create_database(), as.vector))
testthat::test_that("ORM schema generation", {
    testthat::expect_equal(requests[order(requests)], c(
        "CREATE TABLE  adduct (id INTEGER PRIMARY KEY, name TEXT, mass FLOAT, charge INTEGER, multi INTEGER, formula_add TEXT, formula_ded TEXT, sign TEXT, oidscore INTEGER, quasi INTEGER, ips FLOAT)",
        "CREATE TABLE  cluster (id INTEGER PRIMARY KEY, formula TEXT, annotation TEXT, coeff REAL, r_squared REAL, charge INTEGER, mean_rt REAL, score REAL, deviation REAL, status TEXT, adduct TEXT, curent_group INTEGER, pc_group INTEGER, align_group INTEGER, xcms_group INTEGER, sample_id INTEGER, compound_id INTEGER, FOREIGN KEY (sample_id) REFERENCES sample (id), FOREIGN KEY (compound_id) REFERENCES compound (id))",
        "CREATE TABLE  cluster_feature (id INTEGER PRIMARY KEY, cluster_id INTEGER, feature_id INTEGER, FOREIGN KEY (feature_id) REFERENCES feature (id), FOREIGN KEY (cluster_id) REFERENCES cluster (id))",
        "CREATE TABLE  compound (id INTEGER PRIMARY KEY, name TEXT, common_name TEXT, formula TEXT, charge INTEGER, date TEXT, mz REAL)",
        "CREATE TABLE  feature (id INTEGER PRIMARY KEY, mz FLOAT, mz_min FLOAT, mz_max FLOAT, rt FLOAT, rt_min FLOAT, rt_max FLOAT, int_o FLOAT, int_b FLOAT, max_o FLOAT, iso TEXT, abundance FLOAT)",
        "CREATE TABLE  instrument (id INTEGER PRIMARY KEY, model TEXT, manufacturer TEXT, analyzer TEXT, dector_type TEXT)",
        "CREATE TABLE  instrument_config (id INTEGER PRIMARY KEY, resolution TEXT, agc_target TEXT, maximum_IT TEXT, number_of_scan_range TEXT, scan_range TEXT, version TEXT)",
        "CREATE TABLE  sample (id INTEGER PRIMARY KEY, name TEXT, path TEXT, raw_path TEXT, polarity TEXT, raw BLOB, instrument_id INTEGER, instrument_config_id INTEGER, software_id INTEGER, FOREIGN KEY (instrument_id) REFERENCES instrument (id), FOREIGN KEY (instrument_config_id) REFERENCES instrument_config (id), FOREIGN KEY (software_id) REFERENCES software (id))",
        "CREATE TABLE  software (id INTEGER PRIMARY KEY, name TEXT, version TEXT)"
    ))
})

tribromophenol <- orm$compound(
    name='Tribromophenol',
    common_name='Tribromophenol',
    formula='C6H3Br3O1',
    charge=0,
    date='2017-09-01',
    mz=329.771356516
)
dibromophenol <- orm$compound(
    name='Dibromophenol',
    common_name='Dibromophenol',
    formula='C6H4Br2O1',
    charge=0,
    date='2017-09-01',
    mz=251.86084364800001367
)
trichlorophenol <- orm$compound(
    name='Trichlorophenol',
    common_name='Trichlorophenol',
    formula='C6H3Cl3O1',
    charge=0,
    date='2017-09-01',
    mz=195.92494784600000911
)
dichlorophenol <- orm$compound(
    name='Dichlorophenol',
    common_name='Dichlorophenol',
    formula='C6H4Cl2O1',
    charge=0,
    date='2017-09-01',
    mz=161.96392016799998714
)

testthat::test_that("ORM models saving in database", {
    testthat::expect_equal(
        tribromophenol$save(return_request=TRUE),
        "INSERT INTO compound ( name, common_name, formula, charge, date, mz ) VALUES ( 'Tribromophenol', 'Tribromophenol', 'C6H3Br3O1', 0, '2017-09-01', 329.771356516 ) "
    )
    testthat::expect_equal(
        dibromophenol$save(return_request=TRUE),
        "INSERT INTO compound ( name, common_name, formula, charge, date, mz ) VALUES ( 'Dibromophenol', 'Dibromophenol', 'C6H4Br2O1', 0, '2017-09-01', 251.860843648 ) "
    )
    testthat::expect_equal(
        trichlorophenol$save(return_request=TRUE),
        "INSERT INTO compound ( name, common_name, formula, charge, date, mz ) VALUES ( 'Trichlorophenol', 'Trichlorophenol', 'C6H3Cl3O1', 0, '2017-09-01', 195.924947846 ) "
    )
    testthat::expect_equal(
        dichlorophenol$save(return_request=TRUE),
        "INSERT INTO compound ( name, common_name, formula, charge, date, mz ) VALUES ( 'Dichlorophenol', 'Dichlorophenol', 'C6H4Cl2O1', 0, '2017-09-01', 161.963920168 ) "
    )
})

testthat::test_that("ORM model loading", {
    loaded_dichlorophenol <- orm$compound()$load_by(name="Dichlorophenol", mz=161.96392016799998714)
    testthat::expect_equal(loaded_dichlorophenol$fields__, dichlorophenol$fields__)
    testthat::expect_true(loaded_dichlorophenol == dichlorophenol)
    compounds <- orm$compound()$load_by(
        orm$WHERE_CLAUSE(
            field=orm$compound()$table_field(field="name"),
            operator=orm$OPERATORS$EQ,
            value="Dichlorophenol",
            next_connector=orm$LOGICAL_CONNECTORS$OR,
            next_clause=orm$WHERE_CLAUSE(
                field=orm$compound()$table_field(field="name"),
                operator=orm$OPERATORS$EQ,
                value="Trichlorophenol"
            )
        ), orm$WHERE_CLAUSE(
            field=orm$compound()$table_field(field="mz"),
            operator=orm$OPERATORS$GE,
            value=150,
        )
    )
    testthat::expect_true(
        compounds[[1]] == trichlorophenol &&
        compounds[[2]] == dichlorophenol
    )
})

testthat::test_that("ORM model updating", {
    testthat::expect_equal(
        (
            dichlorophenol
            $set_charge(1)
            $set_mz(161.96392016799998714)
            $set_name("Dichlorophenouuul")
            $save(return_request=TRUE)
        ),
        "UPDATE compound SET charge = 1, name = 'Dichlorophenouuul' WHERE 'compound'.'id' == 4"
    )
})



## to be sure that our SQL-I protection works, we must create an
## unsecured orm, and then test that the flaw works.
## Finaly test that it doesn't work anymore with a secured (normal) orm.
orm$with_unsafe_mode__({
    orm$compound(
        name="'unsecured_1'",
        common_name="'trichlorophenol', 456.789) --",
        mz=123.456
    )$save()
})

result <- orm$compound()$load_by(name="unsecured_1")

testthat::test_that("Unsecured ORM SQL Injections success", {
    testthat::expect_equal(result$get_common_name(), "trichlorophenol")
    testthat::expect_equal(result$get_mz(), 456.789)
})

orm$compound(
    name="secured_1",
    common_name="'trichlorophenol', 456.789) --",
    mz=123.456
)$save()
result <- orm$compound()$load_by(name="secured_1")

testthat::test_that("Secured ORM SQL Injections prevented", {
    testthat::expect_equal(result$common_name, "'trichlorophenol', 456.789) --")
    testthat::expect_equal(result$mz, 123.456)
})

orm$disconnect()
