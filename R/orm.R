

#' Returns asked method or attribute.
#' If the attribute is a model name, it is returned as if the model was
#' a attribute of the ORM instance.
#' @param x A orm instance
#' @param name The name of the attribute/method
setMethod("$", "ORM", function(x, name) {
    model <- selectMethod(
        "$", "envRefClass"
    )(x, ".self")[["model_objects_"]][[name]]
    if (is.null(model)) {
        return (selectMethod("$", "envRefClass")(x, substitute(name)))
    }
    return (model)
})

ORM$methods(initialize=function(
    database_path=NULL,
    model_definitions=NULL,
    connect=TRUE,
    in_memory=FALSE
) {
    "
    database_path: A string that represent the location of the database
    to connect to.
    model_definitions: A list of ModelDefinition instances, that defines
    the database schema.
    connect: A boolean telling weither the orm will try to connect to the
    database during instantiation or not.
    in_memory: A boolean telling weither database is in memory or on disc.
    "
    .self$escape_values__must_be_true__ <- TRUE
    .self$in_memory <- in_memory
    if (!is.null(model_definitions)) {
        .self$models(model_definitions)
    }
    if (is.null(database_path)) {
        .self$database_path <- ""
    } else {
        .self$database_path <- database_path
    }
    if (connect) {
        .self$connect()
    }
    .self$execution_context <- list()
    .self$request_pool <- list()
    .self$OPERATORS <- list(
        GE=">=", LE="<=",
        GT=">",  LT="<",
        EQ="==", NE="!=",
        IN="IN", LIKE="LIKE"
    )
    .self$LOGICAL_CONNECTORS <- list(
        AND="AND", OR="OR"
    )
    .self$IF_NO_EXISTS <- "IF NOT EXISTS"
    .self$CREATE_TABLE_TEMPLATE <- "
        CREATE TABLE
        {{if_no_exists}}
            {{table}} (id INTEGER PRIMARY KEY, {{fields}})
    "
    .self$CREATE_LINKAGE_TABLE_TEMPLATE <- "
        CREATE TABLE
        {{if_no_exists}}
            {{table}} (id INTEGER PRIMARY KEY, {{foreign_keys}})
    "
    .self$SELECT_WHERE_TEMPLATE <- "
        SELECT {{fields}} FROM {{table}} {{join_clause}} {{where_clause}}
    "
    .self$INSERT_WHERE_TEMPLATE <- "
        INSERT INTO {{table}} {{fields}} VALUES {{values}} {{where_clause}}
    "
    .self$UPDATE_WHERE_TEMPLATE <- "
        UPDATE {{table}} SET {{update_values}} {{where_clause}}
    "
    .self$FK_CONSTRAINT_TEMPLATE <- "
        FOREIGN KEY
            ({{fk_name}})
        REFERENCES
            {{table}} ({{foreign_field}})
    "

    ## let's remove spaces and newlines, those are not usefull
    ## if we want to print the generated queries
    for (field in c(
        "CREATE_TABLE_TEMPLATE",
        "CREATE_LINKAGE_TABLE_TEMPLATE",
        "SELECT_WHERE_TEMPLATE",
        "INSERT_WHERE_TEMPLATE",
        "UPDATE_WHERE_TEMPLATE",
        "FK_CONSTRAINT_TEMPLATE"
    )) {
        .self[[field]] <- gsub("\n|(^\\s+)|(\\s+$)", "", .self[[field]])
        .self[[field]] <- gsub("\\s+", " ", .self[[field]])
    }
    return(.self)
})

ORM$methods(is_connected=function() {
    "
    Return TRUE if the orm is connected to the database ; FALSE otherwise.
    "
    return (dbIsValid(.self$connection_))
})

ORM$methods(models=function(models=NULL) {
    "
    With no parameters:
        Returns a named list of the form list(adduct=AdductModel,
        compound=CompoundModel)
        With each model being a class generator used to create models
        you will manipulate.
    With one argument:
        The argument must be a list of ModelDefinition instances, that
        defines the database schema.
        Creates the model classes for to these models, and return them
        in the form of a named list.
    "
    if (!is.null(models)) {
        .self$model_definitions_ <- models
        if (
            is.null(names(.self$model_definitions_)) ||
            length(names(
                .self$model_definitions_
            )) != length(.self$model_definitions_)
        ) {
            names(.self$model_definitions_) <- (
                purrr::map(.self$model_definitions_, function(x)x$table)
            )
        }
        .self$model_objects_ <- list()
        for (definition in .self$model_definitions_) {
            .self$model_objects_[[definition$table]] <- model_builder(
                definition, .self
            )
        }
    }
    return (.self$model_objects_)
})

ORM$methods(connect=function() {
    "
    Call this method to connect the orm to the database.
    Returns TRUE if the orm has connected successfully or if it was
    already connected.
    "
    if ((.self$database_path != "") && (!.self$is_connected())) {
        if (.self$in_memory) {
            path <- "file::memory:"
        } else {
            path <- .self$database_path
        }
        .self$connection_ <- dbConnect(
            SQLite(), path
        )
    }
    return (.self$is_connected())
})

ORM$methods(disconnect=function(remove=FALSE) {
    "
    Call this method to disconnect the orm from the database.
    This method should always be called when the is terminated.
    Returns TRUE if the orm is disconnected or if it was already
    disconnected.
    "
    if (.self$is_connected()) {
        dbDisconnect(.self$connection_)
        if (remove) {
            file.remove(.self$database_path)
        }
    }
    return (!.self$is_connected())
})
ORM$methods(table_field=function(...) {
    return (TableField(.self, ...))
})
ORM$methods(where_clause=function(...) {
    return (WhereClause(.self, ...))
})
ORM$methods(join_clause=function(...) {
    return (JoinClause(.self, ...))
})
ORM$methods(with_unsafe_mode__=function(code) {
    "
    Use this function only if you realy know what you do.
    Deactivates input escaping, execute the expression, and 
    reactivate it.
    Returns the result from the expr
    "
    .self$escape_values__must_be_true__ <- FALSE
    result <- code
    .self$escape_values__must_be_true__ <- TRUE
    return (result)
})
ORM$methods(with_connection=function(code) {
    "
    One parameter: a block of code (expression)
    The orm connects to the database, executes the expression and then
    disconnect from the database.
    Returns the result of the exprssion.
    "
    if (!.self$is_connected()) {
        .self$connect()
        if (!.self$is_connected()) {
            stop("Could not connect to the database.")
        }
        res <- code
        .self$disconnect()
    } else {
        ## code is evaluated here
        ## ... or perhaps in "res <- .self$with_connection(code)"
        ## I dunno lol
        res <- code
    }
    return (res)
})

ORM$methods(with_atomic=function(before, then) {
    "
    The `before` parameter and the `then` parameter are expressions.
    The orm will execute your `before` code while ensuring that the requests
    will be in a atomic transaction, and then, call your `after` block.
    A context is accessible in both your expression as the
    `execution_context` attribute of the orm (orm$execution_context).
    You can see a code example at the end of this helpstring.

    More details:
    The orm creates an execution context accessible in your expressions:
    orm$execution_context
    It executes the `before` expression between a 'BEGIN TRANSACTION' and
    a 'COMMIT' to ensure the atomicity of the requests executed in your
    `before` block.
    The result of you `before` block is capured in `orm$execution_context$rs`.
    Then, the orm executes your `then` expression. The context is still
    available in the `then` expression.
    You can manipulate the result of your first block
    through `orm$execution_context$rs`.
    Your codes must call the RSQLite::dbClearResult method to clear the
    SQLiteResult results if any.

    An example is always more clear:\\preformatted{
        orm$with_atomic({
            ## the context is shared across your expressions
            context <- orm$execution_context
            ## do some requests that needs to be atomic
            ...
            orm$get_query(\"SELECT 42 as the_response\")
        }, {
            context <- orm$execution_context
            print(context$rs$the_response)  ## prints '42'

            ## necessay to free the memory allocated for the SQLiteResult
            dbClearResult(context$rs$the_response)
        })
        }

    For a more concreet example, see the `save` method of ModelMeta:
    At the end of the ModelMeta$save method, we need to retrieve the ID
    of the newly inserted entry. This is where we need to have an
    atomic operation:
        - insert the new entry (it has no id)
        - retrieve the generated id (generated by the database)
    This way we can know the object's id if it didn't have any.
    These requests were needed to be atomic to prevent a second `insert`
    request between the first one and the id retrival.
    "
    .self$execution_context <- list()
    RSQLite::dbClearResult(.self$send_statement("BEGIN TRANSACTION"))
    .self$execution_context$rs <- force(before)
    RSQLite::dbClearResult(.self$send_statement("COMMIT"))
    result <- force(then)
    .self$execution_context <- list()
    return (result)
})

ORM$methods(with_query=function(request, expr) {
    "
    request: a request to use the `send_query` to.
    expr: an expression called after the request is executed.
    This method is a shortcut to execute a `send_query` and use the
    result in your expression.
    The SQLiteResult object will automatically be freed for you.
    It prevents users from forgeting to clear their request results.

    An example is always more clear:\\preformatted{
        with_query('SELECT * from compounds', {
            ## prints the result of the query
            print(orm$execution_context$rs)
        })
        ## the query result is automatically freed, you don't have to
        ## bother anymore with this operation.
    }
    "
    return (.self$with__(.self$send_query, request, expr))
})

ORM$methods(with_statement=function(request, expr) {
    "
    request: a request to use the `send_statement` to.
    expr: an expression called after the request is executed.
    This method is a shortcut to execute a `send_statement` and use the
    result in your expression.
    The SQLiteResult object will automatically be freed for you.
    It prevents users from forgeting to clear their request results.

    An example is always more clear:\\preformatted{
        with_query('SELECT * from compounds', {
            ## prints the result of the query
            print(orm$execution_context$rs)
        })
        ## the query result is automatically freed, you don't have to
        ## bother anymore with this operation.
    }
    "
    return (.self$with__(.self$send_statement, request, expr))
})

ORM$methods(with__=function(method, request, expr) {
    "
    Internal method. Do not use.
    Generic method called by `with_statement` and `with_query`
    May disapear or change quickly. Don't rely on it.
    "
    rs <- method(request)
    .self$execution_context <- list(rs=rs)
    result <- expr
    RSQLite::dbClearResult(rs)
    .self$execution_context <- list()
    return (result)
})

ORM$methods(execute=function(request) {
    "
    Calls RSQLite::dbExecute with the curent connection.
    "
    return (RSQLite::dbExecute(.self$connection_, request))
})

ORM$methods(send_query=function(request) {
    "
    Calls RSQLite::dbSendQuery with the curent connection.
    "
    return (RSQLite::dbSendQuery(.self$connection_, request))
})

ORM$methods(get_query=function(request) {
    "
    Calls RSQLite::dbGetQuery with the curent connection.
    "
    return (RSQLite::dbGetQuery(.self$connection_, request))
})

ORM$methods(send_statement=function(request) {
    "
    Calls RSQLite::dbSendStatement with the curent connection.
    "
    return (RSQLite::dbSendStatement(.self$connection_, request))
})

ORM$methods(escape=function(input) {
    "
    Calls dbQuoteLiteral with the curent connection.
    http://xkcd.com/327/
    "
    if (.self$escape_values__must_be_true__) {
        return (dbQuoteLiteral(
            .self$connection_, input
        ))
    }
    return (input)
})

ORM$methods(recreate_database=function(no_exists=TRUE) {
    "
    Disconnects from the curent connection and remove the file.
    Re-connects and create the database, with the curent models.
    "
    .self$disconnect(remove=TRUE)
    .self$connect()
    return (.self$create_database(no_exists=no_exists))
})

ORM$methods(create_database=function(no_exists=TRUE) {
    "
    Create the database with the curent models.
    "
    created_mutual <- list()
    for(schema in .self$model_definitions_) {
        if (length(schema$one) == 0) {
            request <- .self$create_table_without_fk_request(
                schema, no_exists=no_exists
            )
            .self$add_to_request_pool(request)
        }
        if(length(schema$many) > 0) {
            for (mutual in schema$many) {
                if (schema$table < mutual) {
                    tables <- c(schema$table, mutual)
                } else {
                    tables <- c(mutual, schema$table)
                }
                link_id <- paste(tables, collapse=";")
                if (is.null(created_mutual[[link_id]])) {
                    created_mutual[[link_id]] <- TRUE
                    other <- .self$model_definitions_[[mutual]]
                    ## we create a linkage table using tables name
                    ## ordered alphabetically, si it's reproducible.
                    if (schema$table < other$table) {
                        request <- .self$create_linkage_table_request(
                            schema, other, no_exists=no_exists
                        )
                    } else {
                        request <- .self$create_linkage_table_request(
                            other, schema, no_exists=no_exists
                        )
                    }
                    .self$add_to_request_pool(request)
                }
            }
        }
    }
    for(schema in .self$model_definitions_) {
        if (length(schema$one) > 0) {
            request <- .self$create_table_with_fks_request(
                schema, no_exists=no_exists
            )
            .self$add_to_request_pool(request)
        }
    }
    requests <- .self$request_pool
    .self$execute_request_pool()
    return (requests)
})

ORM$methods(add_to_request_pool=function(request) {
    "
    Add a request string to the pool of requests.
    "
    .self$request_pool[[length(request_pool)+1]] <- request
})

ORM$methods(execute_request_pool=function(flush=TRUE) {
    "
    Executes all the requests in the pool, and then empy the pool.
    "
    for (request in .self$request_pool) {
        .self$execute(request)
    }
    if (flush == TRUE) {
        .self$delete_request_pool()
    }
})

ORM$methods(delete_request_pool=function() {
    "
    Empty the request pool.
    "
    .self$request_pool <- list()
})

ORM$methods(create_table_without_fk_request=function(schema, no_exists=TRUE) {
    "
    Internal method. Do not use.
    Create the request string for the given model (mustn't have fks).
    May disapear or change quickly. Don't rely on it.
    "
    fields <- build_fields_declaration(schema)
    if_no_exists <- c(.self$IF_NO_EXISTS, "")[[no_exists+1]]
    return (fill_template(
        CREATE_TABLE_TEMPLATE,
        table=schema$table,
        fields=fields,
        if_no_exists=if_no_exists
    ))
})

ORM$methods(create_linkage_table_request=function(
    schema, other, no_exists=TRUE
) {
    "
    Internal method. Do not use.
    Create the request string for the linkage table between the  given models.
    May disapear or change quickly. Don't rely on it.
    "
    if_no_exists <- c(.self$IF_NO_EXISTS, "")[[no_exists+1]]
    fk_definitions <- rep(list("INTEGER"), 2)
    names(fk_definitions) <- list(
        paste(schema$table, "id", sep="_"),
        paste(other$table, "id", sep="_")
    )
    fields <- paste(
        .self$build_fields_declaration(list(fields=fk_definitions)),
        .self$build_fk_constraint(other$table),
        .self$build_fk_constraint(schema$table),
        sep=", "
    )
    return (fill_template(
        CREATE_LINKAGE_TABLE_TEMPLATE,
        table=paste(schema$table, other$table, sep="_"),
        foreign_keys=fields,
        if_no_exists=if_no_exists
    ))
})

ORM$methods(create_table_with_fks_request=function(schema, no_exists=TRUE) {
    "
    Internal method. Do not use.
    Create the request string for the given model (must have one/some fks).
    May disapear or change quickly. Don't rely on it.
    "
    fk_constraints <- paste(map(schema$one, .self$build_fk_constraint), collapse=", ")
    fields <- build_fields_declaration(schema)
    if_no_exists <- c(.self$IF_NO_EXISTS, "")[[no_exists+1]]
    return (fill_template(
        .self$CREATE_TABLE_TEMPLATE,
        table=schema$table,
        fields=sprintf("%s, %s", fields, fk_constraints),
        if_no_exists=if_no_exists
    ))
})

ORM$methods(create_select_request=function(
    table="", fields=NULL, join=NULL, where=NULL
) {
    "
    Internal method. Do not use.
    Create the request string to select some fields from one table,
    can have a `where` clause.
    May disapear or change quickly. Don't rely on it.
    "
    result <- (fill_template(
        .self$SELECT_WHERE_TEMPLATE,
        table=table,
        fields=.self$build_select_fields(fields, table=table),
        join_clause=.self$build_join_clause(join),
        where_clause=.self$build_where_clause(where)
    ))
    return (result)
})

ORM$methods(create_insert_request=function(
    table="", fields=NULL, values=NULL, where=NULL
) {
    "
    Internal method. Do not use.
    Create the request string to insert some values in one table,
    can have a `where` clause.
    May disapear or change quickly. Don't rely on it.
    "
    if (is.null(fields) || length(fields) == 0) {
        fields <- ""
    } else {
        fields <- sprintf("(%s)", paste(fields, collapse=", "))
    }
    result <- (fill_template(
        .self$INSERT_WHERE_TEMPLATE,
        table=table,
        fields=fields,
        values=.self$build_insert_values(values),
        where_clause=.self$build_where_clause(where)
    ))
    return (result)
})
ORM$methods(create_update_request=function(table="", values=NULL, where=NULL) {
    "
    Internal method. Do not use.
    Create the request string to update some values in one table,
    can have a `where` clause.
    May disapear or change quickly. Don't rely on it.
    "
    # column_1 = new_value_1,
    # column_2 = new_value_2
    if (is.null(values) || length(values) == 0) {
        stop("Cannot update a table with no given values.")
    }
    fields <- names(values)
    update_values <- paste(map(seq_along(fields), function(x){
        sprintf("%s = %s", fields[[x]], .self$escape(values[[x]]))
    }), collapse=", ")
    result <- (fill_template(
        .self$UPDATE_WHERE_TEMPLATE,
        table=table,
        update_values=update_values,
        where_clause=.self$build_where_clause(where)
    ))
    return (result)
})

ORM$methods(build_insert_values=function(values=NULL) {
    "
    Internal method. Do not use.
    Create the `values` part of an insert request.
    May disapear or change quickly. Don't rely on it.
    "
    if (is.null(values)) {
        stop("Can't insert into a table with no any value.")
    }
    if (!.self$is_connected()) {
        values <- .self$with_connection({
            map(values, function(x) {
                return (.self$escape(x))
            })
        })
    } else {
        values <- map(values, function(x) {
            return (.self$escape(x))
        })
    }
    return (paste("(", do.call(paste, list(values, collapse=", ")), ")"))
})

ORM$methods(build_select_fields=function(fields, table=NULL) {
    "
    Internal method. Do not use.
    Create the `values` part of a select request.
    May disapear or change quickly. Don't rely on it.
    "
    if (is.null(fields) || length(fields) == 0) {
        fields <- list("*")
    }
    if (is.null(table) || table == "") {
        return (do.call(paste, list(fields, collapse=", ")))
    } else {
        return (do.call(paste, list(
            purrr::map(fields, function(x)sprintf("%s.%s", table, x)),
            collapse=", "
        )))
    }
})

ORM$methods(build_where_clause=function(where=NULL, sub=FALSE) {
    "
    Internal method. Do not use.
    Create the `where` part of a request.
    May disapear or change quickly. Don't rely on it.
    "
    if (is.null(where)) {
        return ("")
    }
    if (sub) {
        result <- list()
    } else {
        result <- list("WHERE")
    }
    previous.is.clause <- FALSE
    for (clause in where) {
        if (is.character(clause)) {
            if (!any(grepl(clause, .self$LOGICAL_CONNECTORS, fixed=TRUE))) {
                stop(paste(
                    "Malformed \"where\" clause: unknown logical",
                    "connector: ", clause
                ))
            }
            if(previous.is.clause) {
                built_clause <- clause
                previous.is.clause <- FALSE
            } else {
                stop(paste(
                    "Malformed \"where\" clause: logical",
                    "operator after another one, or after nothing",
                    sep=" "
                ))
            }
        } else {
            if (previous.is.clause) {
                stop(paste(
                    "Malformed \"where\" clause: two clauses",
                    "without logical operator (AND/OR/...) between",
                    "them", sep=" "
                ))
            }
            previous.is.clause <- TRUE
            if (is(clause, "WhereClause")) {
                built_clause <- sprintf("(%s)", clause$as.request())
            } else {
                if (!.self$is_connected()) {
                    built_clause <- .self$with_connection({
                        built_clause <- paste(
                            clause$field$as.request(),
                            clause$operator,
                            .self$escape(clause$value)
                        )
                    })
                } else {
                    built_clause <- built_clause <- paste(
                        clause$field$as.request(),
                        clause$operator,
                        .self$escape(clause$value)
                    )
                }
            }
        }
        result[[length(result)+1]] <- built_clause
    }
    return (paste(result, collapse=" "))
})

ORM$methods(build_join_clause=function(join) {
    return (paste(join, collapse=" "))
})

ORM$methods(fill_template=function(template, ...) {
    "
    Internal method. Do not use.
    Fill the given request template with the given additionnal parameters.
    May disapear or change quickly. Don't rely on it.
    "
    replacements <- list(...)
    for (string in names(replacements)) {
        replacement <- replacements[string]
        string <- sprintf("{{%s}}", string)
        template <- gsub(string, replacement, template, fixed=TRUE)
    }
    return (template)
})

ORM$methods(build_fields_declaration=function(schema) {
    "
    Internal method. Do not use.
    Create the `fields` part of a create table request.
    May disapear or change quickly. Don't rely on it.
    "
    field_list <- c()
    fields <- schema[["fields"]]
    field_names <- names(fields)
    for (i in seq_along(fields)) {
        ## id is defined into the template, so we ignore it.
        if (field_names[[i]] != "id") {
            type <- fields[[i]]
            field_list[[length(field_list)+1]] <- paste(field_names[[i]], type)
        }
    }
    return (paste(field_list, collapse=", "))
})

ORM$methods(build_fk_constraint=function(reference, foreign_field="id") {
    "
    Internal method. Do not use.
    Create the `fk restrictions` part of a create table request.
    May disapear or change quickly. Don't rely on it.
    "
    return (fill_template(
        .self$FK_CONSTRAINT_TEMPLATE,
        fk_name=sprintf("%s_id", reference),
        table=reference,
        foreign_field=foreign_field
    ))
})



OperatorClause <- setRefClass(
    "OperatorClause", fields=c(left="character", right="character", operator="character"),
    methods=list(initialize=function(orm=NULL, left="", right="", operator="=") {
        if (is.null(orm)) {
            return (.self)
        }
        .self$left <- left
        .self$right <- right
    }, as.request=function() {
        return (sprintf(
            "%s %s %s", .self$left, .self$operator, .self$right
        ))
    }, show=function(){cat(.self$as.request())})
)


TableField <- setRefClass(
    "TableField", fields=c(table="character", field="character"),
    methods=list(initialize=function(orm=NULL, table="", field="") {
        if (is.null(orm)) {
            return (.self)
        }
        .self$table <- table
        .self$field <- field
    }, as.request=function() {
        return (sprintf("'%s'.'%s'", .self$table, .self$field))
    }, show=function(){cat(.self$as.request())})
)


JoinClause <- setRefClass(
    "JoinClause", fields=c(table="character", on="OperatorClause"),
    methods=list(initialize=function(orm=NULL, table="", on=NULL) {
        if (is.null(orm) || is.null(on)) {
            return (.self)
        }
        .self$table <- table
        .self$on <- on
    }, as.request=function() {
        return (sprintf("JOIN %s ON %s", table, on$as.request()))
    }, show=function(){cat(.self$as.request())})
)

WhereClause <- setRefClass(
    "WhereClause", fields=c(
        field="TableField",
        operator="character",
        value="character",
        next_connector="character",
        next_clause="character"
    ),
    methods=list(initialize=function(
        orm=NULL, field="", operator="", value="",
        next_connector=NULL, next_clause=NULL
    ){
        if (is.null(orm)) {
            return (.self)
        }
        if (!any(grepl(
            sprintf("^%s$", operator),
            orm$OPERATORS,
            perl=TRUE,
            ignore.case=TRUE
        ))) {
            stop(sprintf(
                "Malformed Where Clause: Bad operator: %s", operator
            ))
        }
        .self$field <- field
        .self$operator <- operator
        .self$value <- orm$escape(value)
        if (!is.null(next_connector) && !is.null(next_clause)) {
            if (!any(grepl(
                sprintf("^%s$", next_connector),
                orm$LOGICAL_CONNECTORS,
                perl=TRUE,
                ignore.case=TRUE
            ))) {
                stop(paste0(
                    "Unknown logical connector between this where ",
                    "clause and the following: %s"
                ), next_connector)
            }
            .self$next_connector <- next_connector
            .self$next_clause <- next_clause$as.request()
        } else {
            .self$next_connector <- "NONE"
        }
    }, as.request=function(){
        if (.self$next_connector == "NONE") {
            return (sprintf(
                "%s %s %s",
                .self$field$as.request(),
                .self$operator,
                .self$value
            ))
        }
        return (sprintf(
            "%s %s %s %s %s",
            .self$field$as.request(),
            .self$operator,
            .self$value,
            .self$next_connector,
            .self$next_clause
        ))
    }, show=function(){
        return(print(.self$as.request()))
    })
)