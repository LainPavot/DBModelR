

setMethod("==", signature("ModelMeta", "ModelMeta"), function(e1, e2) {
    if(!is(e1, class(e2))) {
        return (FALSE)
    }
    e1_fields <- e1$fields__
    e2_fields <- e2$fields__
    if (length(e1_fields)!= length(e2_fields)) {
        return (FALSE)
    }
    for (field in names(e1$fields__)) {
        if (
            is.null(e2_fields[[field]]) ||
            e1_fields[[field]] != e2_fields[[field]]
        ) {
            return (FALSE)
        }
    }
    return (TRUE)
})


ModelMeta$methods(initialize=function(...) {
    "\\cr
    "
    callSuper(...)
    .self$modified__ <- list()
    .self$table__ <- "meta"
    .self$fields__ <- list()
    .self$model_name__ <- "MetaModel"
    .self$orm__ <- ORM()
    .self$loaded__ <- FALSE
    .self$sql_model__ <- ModelDefinition(
        table=.self$table__,
        fields=.self$fields__
    )
})

ModelMeta$methods(join_clause=function(table, on) {
    return (.self$orm__$join_clause(
        table=table, on=on
    ))
})

ModelMeta$methods(table_field=function(field) {
    "\
    "
    if (is.null(.self$fields__[[field]])) {
        stop(sprintf(
            "Bad field name <%s> for table <%s>",
            field,
            .self$table__
        ))
    }
    return (.self$orm__$table_field(table=.self$table__, field=field))
})


ModelMeta$methods(show=function() {
    "\\cr
    "
    cat(paste(
        sprintf("<%s [id: %d]>: ", .self$table__, .self$get_id()),
        do.call(paste, c(map(
                Filter(function(x){x!="id"}, names(.self$fields__)),
                function(field) {
                    value <- .self[[field]]
                    if (is.character(value)) {
                        fmt <- "[%s: \"%s\"]"
                    } else {
                        fmt <- "[%s: %s]"
                    }
                    return (sprintf(fmt, field, value))
                }
        ), sep="\n  ")), "",
        sep="\n  "
    ))
})


ModelMeta$methods(load=function(id){
    "\\cr
    "
    return(.self$load_by(id=id))
})


ModelMeta$methods(load_by=function(...){
    "\\cr
    "
    fields <- list(...)
    where <- list()
    join <- list()
    field_names <- names(fields)
    for (i in seq_along(fields)) {
        field <- field_names[[i]]
        value <- fields[[i]]
        if (is(value, "WhereClause")) {
            where[[length(where)+1]] <- value
        } else if (is(value, "JoinClause")) {
            join[[length(join)+1]] <- value
        } else {
            if (is(field, "MetaModel")) {
                class_name <- class(field)
                field <- field$get_id()
                if (field == -1) {
                    stop(sprintf(paste0(
                        "The %s has never been saved to the database, and",
                        "so has no id. You must save the it before."
                    )), class_name)
                }
            } else if (is.null(.self$fields__[[field]])) {
                stop(sprintf(
                    "Field <%s> does not exists for table <%s>",
                    field, .self$table__
                ))
            }
            where[[length(where)+1]] <- list(
                field=.self$orm__$table_field(
                    table=.self$table__, field=field
                ),
                value=value,
                operator=.self$orm__$OPERATORS$EQ
            )
        }
        where[[length(where)+1]] <- .self$orm__$LOGICAL_CONNECTORS$AND
    }
    ## we remove the last "AND" operator
    where[[length(where)]] <- NULL
    request <- .self$orm__$create_select_request(
        table=.self$table__,
        fields=names(.self$fields__),
        join=join,
        where=where
    )
    return (.self$load_from_request__(request))
})


ModelMeta$methods(load_from_request__=function(request) {
    "\\cr
    "
    result <- (.self$orm__$with_query(request, {
        context <- .self$orm__$execution_context
        results <- RSQLite::dbFetch(context$rs)
        if (RSQLite::dbGetRowCount(context$rs) == 0) {
            result <- NULL
        } else if (RSQLite::dbGetRowCount(context$rs) > 1) {
            result <- .self$load_multiple_from_data__(results)
        } else {
            result <- .self$load_one_from_data__(results)
        }
        ## return
        result
    }))
    return (result)
})


ModelMeta$methods(load_multiple_from_data__=function(multiple) {
    "\\cr
    "
    generator <- .self$getRefClass()
    return (purrr::map(1:nrow(multiple), function(row) {
        generator()$load_one_from_data__(multiple[row,])
    }))
})


ModelMeta$methods(load_one_from_data__=function(row) {
    "\\cr
    "
    for (field in names(row)) {
        .self[[field]] <- row[[field]]
    }
    .self$modified__ <- list()
    .self$loaded__ <- TRUE
    return (.self)
})


ModelMeta$methods(save=function(bulk=FALSE, return_request=FALSE) {
    "\\cr
    "
    ## bulk is not used for the moment ; too complicated, and
    ## perhaps useless in XSeeker:
    ## it saves in a pool the models to save and when the user
    ## decides to save them, they're all saved with only one request
    if (bulk == FALSE) {
        if (length(.self$modified__) == 0) {
            return (if (return_request) "" else .self)
        }
        values <- list()
        fields <- names(.self$modified__)
        for (field in fields) {
            values[[field]] <- .self[[field]]
        }
        if (.self$get_id() == -1) {
            request <- .self$orm__$create_insert_request(
                table=.self$table__, fields=fields, values=values
            )
            new_row <- TRUE
        } else {
            request <- .self$orm__$create_update_request(
                table=.self$table__, values=values, where=list(
                    list(
                        field=.self$orm__$table_field(
                            table=.self$table__, field="id"
                        ),
                        value=.self$get_id(),
                        operator=.self$orm__$OPERATORS$EQ
                    )
                )
            )
            new_row <- FALSE
        }
    } else {
        stop(sprintf("%s$save(bulk=TRUE) is not implemented yet.", model_name__))
    }
    get_id <- (
        if (new_row) "SELECT last_insert_rowid() as id"
        else paste("SELECT", .self$orm__$escape(.self$get_id()), "as id")
    )
    .self$orm__$with_atomic(
        before={
            RSQLite::dbClearResult(.self$orm__$send_statement(request))
            .self$orm__$get_query(get_id)
        },
        then={
            context <- .self$orm__$execution_context
            .self$set_id(context$rs$id)
            .self$modified__ <- list()
        }
    )
    return (if (return_request) request else .self)
})
