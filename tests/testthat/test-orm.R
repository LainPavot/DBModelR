
testthat::context("ORM tests")


if (file.exists(DB_PATH)) {
    file.remove(DB_PATH)
}

orm <- ORM(model_definitions=MODELS, connect=FALSE)

connection_parameters <- list()
connection_parameters[[orm$SQLITE]] <- list(
    DB_PATH
)
connection_parameters[[orm$POSTGRESQL]] <- list(
    dbname="dbmodelr_test_db",
    user="testr",
    password="password"
)

connection_parameters[[orm$MYSQL]] <- list(
    dbname="dbmodelr_test_db",
    user="testr",
    password="password"
)

connection_parameters[[orm$MARIADB]] <- list(
    dbname="dbmodelr_test_db",
    user="testr",
    password="password"
)

for (dbms in names(orm$DBMS_PACKAGES)) {

if (file.exists(DB_PATH)) {
    file.remove(DB_PATH)
}

if(tryCatch({
    suppressWarnings(orm$set_dbms(dbms))
    print(sprintf("Testing %s", dbms))
    FALSE
}, error=function(e){
    print(sprintf("Skipping %s test: not installed.", dbms))
    return (TRUE)
}) == TRUE) next

orm$set_connection_parameters(connection_parameters[[dbms]])

if (dbms == orm$SQLITE) {
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
        file.remove(DB_PATH)
    })
} else {
    next
}

connected <- tryCatch({
    orm$connect()
    TRUE
}, error=function(e) {
    print(e)
    print(sprintf(
        "Could not connect to the database using: %s",
        connection_parameters[[dbms]]
    ))
    FALSE
})
if (connected) {
    if (dbms != orm$SQLITE) {
        orm$execute("DROP TABLE if exists adduct,cluster,cluster_feature,compound,feature,instrument,instrument_config,sample,software")
    }


    orm$set_tag("1.2.3", tag_name="version", tag_table_name="database_version")
    testthat::test_that("ORM database tagging", {

        testthat::expect_equal(orm$get_tag("version", tag_table_name="database_version"), "1.2.3")

    })

    requests <- do.call(c, lapply(orm$create_database(), as.vector))
    expected <- c(
        "CREATE TABLE  adduct (id INTEGER PRIMARY KEY, name TEXT, mass FLOAT, charge INTEGER, multi INTEGER, formula_add TEXT, formula_ded TEXT, sign TEXT, oidscore INTEGER, quasi INTEGER, ips FLOAT)",
        "CREATE TABLE  cluster_feature (id INTEGER PRIMARY KEY, cluster_id INTEGER, feature_id INTEGER, FOREIGN KEY (feature_id) REFERENCES feature (id), FOREIGN KEY (cluster_id) REFERENCES cluster (id))",
        "CREATE TABLE  cluster (id INTEGER PRIMARY KEY, formula TEXT, annotation TEXT, coeff REAL, r_squared REAL, charge INTEGER, mean_rt REAL, score REAL, deviation REAL, status TEXT, adduct TEXT, curent_group INTEGER, pc_group INTEGER, align_group INTEGER, xcms_group INTEGER, sample_id INTEGER, compound_id INTEGER, FOREIGN KEY (sample_id) REFERENCES sample (id), FOREIGN KEY (compound_id) REFERENCES compound (id))",
        "CREATE TABLE  compound (id INTEGER PRIMARY KEY, name TEXT, common_name TEXT, formula TEXT, charge INTEGER, date TEXT, mz REAL)",
        "CREATE TABLE  feature (id INTEGER PRIMARY KEY, mz FLOAT, mz_min FLOAT, mz_max FLOAT, rt FLOAT, rt_min FLOAT, rt_max FLOAT, int_o FLOAT, int_b FLOAT, max_o FLOAT, iso TEXT, abundance FLOAT)",
        "CREATE TABLE  instrument_config (id INTEGER PRIMARY KEY, resolution TEXT, agc_target TEXT, maximum_IT TEXT, number_of_scan_range TEXT, scan_range TEXT, version TEXT)",
        "CREATE TABLE  instrument (id INTEGER PRIMARY KEY, model TEXT, manufacturer TEXT, analyzer TEXT, dector_type TEXT)",
        "CREATE TABLE  sample (id INTEGER PRIMARY KEY, name TEXT, path TEXT, raw_path TEXT, polarity TEXT, raw BLOB, instrument_id INTEGER, instrument_config_id INTEGER, software_id INTEGER, FOREIGN KEY (instrument_id) REFERENCES instrument (id), FOREIGN KEY (instrument_config_id) REFERENCES instrument_config (id), FOREIGN KEY (software_id) REFERENCES software (id))",
        "CREATE TABLE  software (id INTEGER PRIMARY KEY, name TEXT, version TEXT)"
    )
    expected <- expected[order(expected)]
    testthat::test_that("ORM schema generation", {
        testthat::expect_equal(requests[order(requests)], expected)
    })

    tribromophenol <- orm$compound(
        name="Tribromophenol",
        common_name="Tribromophenol",
        formula="C6H3Br3O1",
        charge=0,
        date="2017-09-01",
        mz=329.771356516
    )
    dibromophenol <- orm$compound(
        name="Dibromophenol",
        common_name="Dibromophenol",
        formula="C6H4Br2O1",
        charge=0,
        date="2017-09-01",
        mz=251.86084364800001367
    )
    trichlorophenol <- orm$compound(
        name="Trichlorophenol",
        common_name="Trichlorophenol",
        formula="C6H3Cl3O1",
        charge=0,
        date="2017-09-01",
        mz=195.92494784600000911
    )
    dichlorophenol <- orm$compound(
        name="Dichlorophenol",
        common_name="Dichlorophenol",
        formula="C6H4Cl2O1",
        charge=0,
        date="2017-09-01",
        mz=161.96392016799998714
    )

    testthat::test_that("ORM models saving in database", {
        testthat::expect_equal(
            tribromophenol$save(return_request=TRUE),
            "INSERT INTO compound (name, common_name, formula, charge, date, mz) VALUES ('Tribromophenol', 'Tribromophenol', 'C6H3Br3O1', 0, '2017-09-01', 329.771356516) "
        )
        testthat::expect_equal(
            dibromophenol$save(return_request=TRUE),
            "INSERT INTO compound (name, common_name, formula, charge, date, mz) VALUES ('Dibromophenol', 'Dibromophenol', 'C6H4Br2O1', 0, '2017-09-01', 251.860843648) "
        )
        testthat::expect_equal(
            trichlorophenol$save(return_request=TRUE),
            "INSERT INTO compound (name, common_name, formula, charge, date, mz) VALUES ('Trichlorophenol', 'Trichlorophenol', 'C6H3Cl3O1', 0, '2017-09-01', 195.924947846) "
        )
        testthat::expect_equal(
            dichlorophenol$save(return_request=TRUE),
            "INSERT INTO compound (name, common_name, formula, charge, date, mz) VALUES ('Dichlorophenol', 'Dichlorophenol', 'C6H4Cl2O1', 0, '2017-09-01', 161.963920168) "
        )
    })

    testthat::test_that("ORM result set conversion methods", {
        result <- capture.output(print(orm$compound()$load_by(name="Tribromophenol")))
        testthat::expect_equal(
            result,
            c(
                '[[1]]',
                '<compound [id: 1]>: ',
                '  [name: "Tribromophenol"]',
                '  [common_name: "Tribromophenol"]',
                '  [formula: "C6H3Br3O1"]',
                '  [charge: 0]',
                '  [date: "2017-09-01"]',
                '  [mz: 329.771356516]',
                '  '
            )

        )
    })

    testthat::test_that("ORM result set iteration methods", {
        rs <- orm$compound()$load_by(date="2017-09-01")
        testthat::expect_equal(length(rs), 4)
        testthat::expect_equal(rs$length(), 4)
        testthat::expect_true(is(as.vector(rs), "vector"))
        testthat::expect_true(all(sapply(
            as.vector(orm$compound()$all()),
            function(x) is(x, "ModelMeta")
        ))[1])
    })

    testthat::test_that("ORM result set fields manipulation methods", {
        rs <- orm$compound()$all()
        testthat::expect_equal(
            rs$field("mz"),
            list(329.771356516, 251.860843648, 195.924947846, 161.963920168)
        )
        testthat::expect_equal(
            rs$field("mz", SIMPLIFY=TRUE),
            c(329.771356516, 251.860843648, 195.924947846, 161.963920168)
        )
        testthat::expect_equal(rs$mean("mz"), 234.88026704450002)
        testthat::expect_equal(rs$median("mz"), 223.892895747)
    })


    testthat::test_that("ORM 'LIKE' selector", {
        rs <- orm$compound()$load_by(name=quote(~"%bromo%"))
        testthat::expect_equal(rs[[1]]$name, "Tribromophenol")
        testthat::expect_equal(rs[[2]]$name, "Dibromophenol")
        testthat::expect_equal(length(rs), 2)
    })

    testthat::test_that("ORM 'NOT LIKE' selector", {
        rs <- orm$compound()$load_by(name=quote(!~"%bromo%"))
        testthat::expect_equal(rs[[1]]$name, "Trichlorophenol")
        testthat::expect_equal(rs[[2]]$name, "Dichlorophenol")
        testthat::expect_equal(length(rs), 2)
    })

    testthat::test_that("ORM 'NOT' selector", {
        rs <- orm$compound()$load_by(name=expression(!"Tribromophenol"))
        testthat::expect_equal(rs[[1]]$name, "Dibromophenol")
        testthat::expect_equal(rs[[2]]$name, "Trichlorophenol")
        testthat::expect_equal(rs[[3]]$name, "Dichlorophenol")
        testthat::expect_equal(length(rs), 3)
        rs <- orm$compound()$load_by(mz=quote(!161.963920168))
        testthat::expect_equal(rs[[1]]$name, "Tribromophenol")
        testthat::expect_equal(rs[[2]]$name, "Dibromophenol")
        testthat::expect_equal(rs[[3]]$name, "Trichlorophenol")
        testthat::expect_equal(length(rs), 3)
    })

    testthat::test_that("ORM 'NOT IN' selector", {
        rs <- orm$compound()$load_by(name=quote(!list("Dibromophenol", "Trichlorophenol")))
        testthat::expect_equal(rs[[1]]$name, "Tribromophenol")
        testthat::expect_equal(rs[[2]]$name, "Dichlorophenol")
        testthat::expect_equal(length(rs), 2)
        name_list <- list("Dibromophenol", "Trichlorophenol")
        rs <- orm$compound()$load_by(name=substitute(!names, list(names=name_list)))
        testthat::expect_equal(rs[[1]]$name, "Tribromophenol")
        testthat::expect_equal(rs[[2]]$name, "Dichlorophenol")
        testthat::expect_equal(length(rs), 2)
    })

    testthat::test_that("ORM 'LT' selector", {
        rs <- orm$compound()$load_by(mz=quote("<"|251))
        testthat::expect_equal(rs[[1]]$name, "Trichlorophenol")
        testthat::expect_equal(rs[[2]]$name, "Dichlorophenol")
        testthat::expect_equal(length(rs), 2)
    })

    testthat::test_that("ORM 'GT' selector", {
        rs <- orm$compound()$load_by(mz=quote(">"|251))
        testthat::expect_equal(rs[[1]]$name, "Tribromophenol")
        testthat::expect_equal(rs[[2]]$name, "Dibromophenol")
        testthat::expect_equal(length(rs), 2)
    })

    testthat::test_that("ORM 'LE' selector", {
        rs <- orm$compound()$load_by(mz=quote("<="|251.860843648))
        testthat::expect_equal(rs[[1]]$name, "Dibromophenol")
        testthat::expect_equal(rs[[2]]$name, "Trichlorophenol")
        testthat::expect_equal(rs[[3]]$name, "Dichlorophenol")
        testthat::expect_equal(length(rs), 3)
    })

    testthat::test_that("ORM 'GE' selector", {
        rs <- orm$compound()$load_by(mz=quote(">="|251.860843648))
        testthat::expect_equal(rs[[1]]$name, "Tribromophenol")
        testthat::expect_equal(rs[[2]]$name, "Dibromophenol")
        testthat::expect_equal(length(rs), 2)
    })


    testthat::test_that("ORM model loading", {
        loaded_dichlorophenol <- orm$compound()$load_by(
            name="Dichlorophenol", mz=161.96392016799998714
        )$first()
        testthat::expect_equal(loaded_dichlorophenol$fields__, dichlorophenol$fields__)
        testthat::expect_true(loaded_dichlorophenol == dichlorophenol)
        compound <- orm$compound()
        compounds <- compound$load_by(
            orm$where_clause(
                field=compound$table_field(field="name"),
                operator=orm$OPERATORS$EQ,
                value="Dichlorophenol",
                next_connector=orm$LOGICAL_CONNECTORS$OR,
                next_clause=orm$where_clause(
                    field=compound$table_field(field="name"),
                    operator=orm$OPERATORS$EQ,
                    value="Trichlorophenol"
                )
            ), orm$where_clause(
                field=compound$table_field(field="mz"),
                operator=orm$OPERATORS$GE,
                value=150
            )
        )
        testthat::expect_true(
            compounds[[1]] == trichlorophenol &&
            compounds[[2]] == dichlorophenol
        )
        compounds <- compound$load_by(
            name=list("Dichlorophenol", "Trichlorophenol"),
            orm$where_clause(
                field=compound$table_field(field="mz"),
                operator=orm$OPERATORS$GE,
                value=150
            )
        )
        testthat::expect_true(
            compounds[[1]] == trichlorophenol &&
            compounds[[2]] == dichlorophenol
        )
    })


    testthat::test_that("ORM model loading with raw OR operator", {
        compound <- orm$compound()
        compounds <- compound$load_by(
            name="Dichlorophenol",
            orm$LOGICAL_CONNECTORS$OR,
            name="Trichlorophenol"
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
            "UPDATE compound SET charge = 1, name = 'Dichlorophenouuul' WHERE (compound.id = 4)"
        )
    })
    orm$sample()$save()

    testthat::test_that("ORM model type coercion", {
        testthat::expect_equal(
            as.list(dichlorophenol, c("name", "common_name", "formula", "charge", "date", "mz")),
            list(
                name="Dichlorophenouuul",
                common_name="Dichlorophenol",
                formula="C6H4Cl2O1",
                charge=1,
                date="2017-09-01",
                mz=161.96392016799998714
            )
        )
        testthat::expect_equal(
            as.matrix(dichlorophenol),
            matrix(
                c("Dichlorophenouuul", "Dichlorophenol", "C6H4Cl2O1", "1", "2017-09-01", as.character(161.96392016799998714), "4"),
                nrow=1, dimnames=list(list(1), c("name", "common_name", "formula", "charge", "date", "mz", "id"))
            )
        )
        testthat::expect_equal(
            as.data.frame(dichlorophenol),
            data.frame(
                name="Dichlorophenouuul", common_name="Dichlorophenol",
                formula="C6H4Cl2O1", charge=1,
                date="2017-09-01", mz=161.96392016799998714, id=4
            )
        )
        testthat::expect_equal(
            as.list(dichlorophenol),
            list(
                name="Dichlorophenouuul", common_name="Dichlorophenol",
                formula="C6H4Cl2O1", charge=1,
                date="2017-09-01", mz=161.96392016799998714, id=4
            )
        )
        testthat::expect_equal(
            as.list(dichlorophenol, c("name", "charge")),
            list(name="Dichlorophenouuul", charge=1)
        )
    })


    ## to be sure that our SQL-I protection works, we must create an
    ## unsecured orm, and then test that the flaw works.
    ## Finaly test that it doesn't work anymore with a secured (normal) orm.
    orm$with_unsafe_mode__({
        orm$compound(
            name="'unsecured_1'",
            common_name="'trichlorophenol', 'C6H3Cl3O1', 0, '2017-09-01', 456.789) --",
            mz=123.456
        )$save()
    })

    result <- orm$compound()$load_by(name="unsecured_1")$first()

    testthat::test_that("Unsecured ORM SQL Injections success", {
        testthat::expect_equal(result$get_common_name(), "trichlorophenol")
        testthat::expect_equal(result$get_mz(), 456.789)
    })

    orm$compound(
        name="secured_1",
        common_name="'trichlorophenol', 456.789) --",
        mz=123.456
    )$save()
    result <- orm$compound()$load_by(name="secured_1")$first()

    testthat::test_that("Secured ORM SQL Injections prevented", {
        testthat::expect_equal(result$common_name, "'trichlorophenol', 456.789) --")
        testthat::expect_equal(result$mz, 123.456)
    })

    orm$disconnect()


    models <- list(
        person=ModelDefinition(
            table="person",
            fields=list(
                name="TEXT",
                family_name="TEXT"
            ), many=list("adress")
            ## One person possibly has multiple adress. Or none...
            ## "many" fields defines the creation of a linkage table
            ## (many_to_many or one_to_many).
            ## "one" fields defines a fk field enforced by a foreign key
            ## restriction (one_to_one)
            ## "many" fields mustn't be duplicated. So, "adress" must not
            ## reference the "person" table.
        ),
        adress=ModelDefinition(
            table="adress",
            fields=list(
                number="INTEGER",
                street="TEXT"
            )
        )
    )

    testthat::test_that("Regular use case", {
        orm <- ORM(model_definitions=models, connect=FALSE)
        orm$set_dbms(dbms)
        orm$set_connection_parameters(connection_parameters[[dbms]])
        orm$connect()

        ## like this, we'll see the requests generated by the orm
        requests <- orm$create_database()
        testthat::expect_equal(
            requests[order(do.call(c, requests))],
            list(
                "CREATE TABLE  adress (id INTEGER PRIMARY KEY, number INTEGER, street TEXT)",
                "CREATE TABLE  adress_person (id INTEGER PRIMARY KEY, adress_id INTEGER, person_id INTEGER, FOREIGN KEY (person_id) REFERENCES person (id), FOREIGN KEY (adress_id) REFERENCES adress (id))",
                "CREATE TABLE  person (id INTEGER PRIMARY KEY, name TEXT, family_name TEXT)"
            )
        )
        ## the tables has been generated, fks and their restrictions has been
        ## defined and linked table has been created if necessary

        ## we've created a person who's name is Alice Smith.
        ## Alice has been saved into the database.
        alice <- orm$person(name="Alice", family_name="smith")$save()

        ## Alice has been successfully added to and loaded from the database.
        testthat::expect_true(orm$person()$load_by(name="Alice")$first() == alice)

        ## The id is 1, because it's the first person to be inserted into the
        ## "person" table.
        ## The orm has generated an "INSERT" query.
        testthat::expect_equal(alice$get_id(), 1)

        ## He's a boy, so he's changed his name to "Bob". Suits him better.
        ## Fields must always be setted with "model$set_field_name(value)".
        ## otherwise the orm will not see the modifications, and will not save
        ## them in the database.
        bob <- alice$set_name("bob")$save()

        ## still 1. Because the orm did not add new database entry.
        ## The orm has generated an "UPDATE" query.
        testthat::expect_equal(bob$get_id(), 1)

        ## prints an empty list()
        ## no adress has been assigned to him for the moment.
        adress <- bob$get_adress()
        testthat::expect_false(is.null(adress))
        testthat::expect_true(is(adress, "ResultSet"))
        testthat::expect_equal(adress$length(), 0)

        ## let's give him a home
        strange_street_name <- "Second street ; -- drop table person"
        bob$add_adress(
            ## the orm sanitizes the user's inputs and prevent sql injections.
            orm$adress(number=42, street=strange_street_name)
        )
        ## The orm detects that the adress object assigned to bob is not saved
        ## yet in the database.
        ## So, the orm will register the adress, and then create a link between
        ## bob and the adress through a linkage table.
        bob$save()


        ## now Bob is happy because he has an adress
        ## prints: list(<adress id: 1> ...) etc.
        testthat::expect_equal(
            lapply(as.vector(adress <- bob$get_adress()), function(x)x$as.character()),
            list(paste(
                "<adress [id: 1]>: ",
                "[number: 42]",
                sprintf("[street: \"%s\"]", strange_street_name),
                "",
                sep="\n  "
            ))
        )

        ## the street name is still somewhat strange...
        ## let's make it less strange
        bob$get_adress(
            ## we select the adress with a strange name...
            street=strange_street_name

        ## we set a more... usual name. And we save it (the adress).
        )[[1]]$set_street("Second street")$save()
        testthat::expect_equal(
            lapply(as.vector(bob$get_adress()), function(x)x$as.character()),
            list(paste(
                "<adress [id: 1]>: ",
                "[number: 42]",
                "[street: \"Second street\"]",
                "",
                sep="\n  "
            ))
        )

        ## never forget to disconnect when your're finished!
        orm$disconnect()

        ## not to forget, there's a little trick:
        ## this call orm$connect()
        ## and at the end of the block, it calls orm$disconnect()
        ## so you never foget to disconnect from the database.
        orm <- ORM(model_definitions=models, connect=FALSE)
        orm$set_dbms(dbms)
        orm$set_connection_parameters(connection_parameters[[dbms]])

        orm$with_connection({
            bob <- orm$person()$load_by(name="bob")$first()
            bob$add_adress(
                ## he's has a second residence
                orm$adress(number=2, street="the squirel's path")
            )$save()
            testthat::expect_equal(
                lapply(as.vector(bob$get_adress()), function(x)x$as.character()),
                list(paste(
                    "<adress [id: 1]>: ",
                    "[number: 42]",
                    "[street: \"Second street\"]",
                    "",
                    sep="\n  "
                ), paste(
                    "<adress [id: 2]>: ",
                    "[number: 2]",
                    "[street: \"the squirel's path\"]",
                    "",
                    sep="\n  "
                ))
            )

            ## finily he decided to live in his second house, and sold the
            ## first one.
            testthat::expect_equal(
                lapply(
                    as.vector(bob$get_adress(street="Second street")),
                    function(x)x$as.character()
                ),
                list(paste(
                    "<adress [id: 1]>: ",
                    "[number: 42]",
                    "[street: \"Second street\"]",
                    "",
                    sep="\n  "
                ))
            )
            bob$remove_adress(bob$get_adress(street="Second street")$first())
            bob$save()
            testthat::expect_equal(
                lapply(as.vector(bob$get_adress()), function(x)x$as.character()),
                list(paste(
                    "<adress [id: 2]>: ",
                    "[number: 2]",
                    "[street: \"the squirel's path\"]",
                    "",
                    sep="\n  "
                ))
            )

        })
        ## now, you're disconnected from the database.

    })
}


}

