#'
#' @import RSQLite
NULL


#' Compare two models using their class and their fields
#' @param e1 A model instance
#' @param e2 A model instance
#' @return The equality between \code{e1} and \code{e2}
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
    "\
    "
    callSuper(...)
    .self$modified__ <- list()
    .self$table__ <- "meta"
    .self$fields__ <- list()
    .self$model_name__ <- "ModelMeta"
    .self$orm__ <- ORM()
    .self$loaded__ <- FALSE
    .self$cache_read__ <- list()
    .self$cache_remove__ <- list()
    .self$cache_add__ <- list()
    .self$sql_model__ <- ModelDefinition(
        table=.self$table__,
        fields=.self$fields__
    )
})

ModelMeta$methods(join_clause=function(...) {
    return (.self$orm__$join_clause(...))
})

ModelMeta$methods(where_clause=function(...) {
    return (.self$orm__$where_clause(...))
})

ModelMeta$methods(operator_clause=function(...) {
    return (.self$orm__$operator_clause(...))
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
    "\
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
    "\
    "
    return(.self$load_by(id=id))
})


ModelMeta$methods(load_by=function(...) {
    "\
    "
    fields <- list(...)
    where <- list()
    add_from <- list()
    join <- list()
    field_names <- names(fields)
    previous_is_where <- FALSE
    distinct <- FALSE
    for (i in seq_along(fields)) {
        field <- field_names[[i]]
        value <- fields[[i]]
        if (is(value, "JoinClause")) {
            join[[length(join)+1]] <- value
            previous_is_where <- FALSE
        } else if (!is.null(field) && field == "add_from") {
            add_from[[length(add_from)+1]] <- value
        } else if (!is.null(field) && field == "distinct") {
            distinct <- value
        } else {
            if (previous_is_where) {
                where[[length(where)+1]] <- .self$orm__$LOGICAL_CONNECTORS$AND
            }
            if (is(value, "WhereClause")) {
                where[[length(where)+1]] <- value
                previous_is_where <- TRUE
            } else {
                if (is(value, "ModelMeta")) {
                    class_name <- class(value)
                    value <- value$get_id()
                    if (value == -1) {
                        stop(sprintf(paste0(
                            "The %s has never been saved to the ",
                            "database, and so has no id. You must ",
                            "save the it before."
                        )), class_name)
                    }
                } else if (is.null(.self$fields__[[field]])) {
                    stop(sprintf(
                        "Field <%s> does not exists for table <%s>",
                        field, .self$table__
                    ))
                }
                where[[length(where)+1]] <- list(
                    field=.self$table_field(field=field),
                    value=value,
                    operator=.self$orm__$OPERATORS$EQ
                )
                previous_is_where <- TRUE
            }
        }
    }
    request <- .self$orm__$create_select_request(
        distinct=distinct,
        table=.self$table__,
        fields=names(.self$fields__),
        join=join,
        where=where,
        additionnal_froms=add_from
    )
    return (.self$load_from_request__(request))
})

ModelMeta$methods(load_join=function(...) {
    join_list <- list(...)
    map(join_list, function(join) {
        .self$orm$JoinClause(
            table=join$table
        )
    })
})

ModelMeta$methods(load_from_request__=function(request) {
    "\
    "
    result <- (.self$orm__$with_query(request, {
        context <- .self$orm__$execution_context
        results <- dbFetch(context$rs)
        if (dbGetRowCount(context$rs) == 0) {
            result <- list()
        } else if (dbGetRowCount(context$rs) > 1) {
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
    "\
    "
    generator <- .self$getRefClass()
    return (map(seq_len(nrow(multiple)), function(row) {
        generator()$load_one_from_data__(multiple[row,])
    }))
})


ModelMeta$methods(load_one_from_data__=function(row) {
    "\
    "
    for (field in names(row)) {
        .self[[field]] <- row[[field]]
    }
    .self$modified__ <- list()
    .self$loaded__ <- TRUE
    return (.self)
})


ModelMeta$methods(save=function(bulk=FALSE, return_request=FALSE) {
    "\
    "
    ## bulk is not used for the moment ; too complicated, and
    ## perhaps useless in XSeeker:
    ## it saves in a pool the models to save and when the user
    ## decides to save them, they're all saved with only one request
    if (bulk == FALSE) {
        if (.self$get_id() == -1) {
            .self$save_added_fk_()
            field_names <- Filter(
                function(x){x!="id"},
                names(.self$fields__)
            )
            request <- .self$orm__$create_insert_request(
                table=.self$table__,
                fields=field_names,
                values=map(field_names, function(x).self[[x]])
            )
            new_row <- TRUE
        } else {
            if (length(.self$modified__) == 0) {
                request <- ""
            } else {
                values <- list()
                fields <- Filter(
                    function(x){x!="id"},
                    names(.self$modified__)
                )
                for (field in fields) {
                    values[[field]] <- .self[[field]]
                }
                request <- .self$orm__$create_update_request(
                    table=.self$table__,
                    values=values,
                    where=list(
                        .self$where_clause(
                            field=.self$table_field(field="id"),
                            value=.self$get_id(),
                            operator=.self$orm__$OPERATORS$EQ
                        )
                    )
                )
            }
            .self$save_added_fk_()
            .self$save_removed_many_()
            new_row <- FALSE
        }
    } else {
        stop(sprintf(
            "%s$save(bulk=TRUE) is not implemented yet.", model_name__
        ))
    }
    if (request != "") {
        get_id <- (
            if (new_row) "SELECT last_insert_rowid() as id"
            else paste("SELECT", .self$orm__$escape(.self$get_id()), "as id")
        )
        .self$orm__$with_atomic(
            before={
                dbClearResult(.self$orm__$send_statement(request))
                .self$orm__$get_query(get_id)
            },
            then={
                context <- .self$orm__$execution_context
                .self$set_id(context$rs$id)
                .self$modified__ <- list()
            }
        )
    }
    ## once the object is updated/created, we update its links
    .self$save_added_many_()
    return (if (return_request) request else .self)
})

ModelMeta$methods(save_added_fk_=function() {
    for (obj in .self$modified__) {
        if (is(obj, "ModelMeta") && obj$get_id() == -1) {
            obj$save()
            .self$cache_read__[[obj$table__]] <- NULL
        }
    }
})

ModelMeta$methods(save_added_many_=function() {
    for (table in names(.self$cache_add__)) {
        for (obj in .self$cache_add__[[table]]) {
            if (obj$get_id() == -1) {
                obj$save()
            }
            .self$create_link_to_(obj)
        }
        .self$cache_add__[[table]] <- list()
        .self$cache_read__[[table]] <- NULL
    }
})

ModelMeta$methods(save_removed_many_=function() {
    requests <- list()
    for (table in names(.self$cache_remove__)) {
        for (obj in .self$cache_remove__[[table]]) {
            requests[[length(requests)+1]] <- .self$unlink_from_(obj)
        }
        .self$cache_read__[[table]] <- NULL
    }
    .self$cache_remove__ <- list()
    return (requests)
})

ModelMeta$methods(unlink_from_=function(other) {
    if (.self$table__ < other$table__) {
        linkage_table_name <- sprintf(
            "%s_%s", .self$table__, other$table__
        )
    } else {
        linkage_table_name <- sprintf(
            "%s_%s", other$table__, .self$table__
        )
    }
    request <- .self$orm__$create_delete_request(
        table=linkage_table_name,
        where=list(
            .self$orm__$where_clause(
                field=.self$orm__$table_field(
                    table=linkage_table_name,
                    field=sprintf("%s_id", .self$table__)
                ),
                operator=.self$orm__$OPERATORS$EQ,
                value=.self$get_id(),
                next_connector=.self$orm__$LOGICAL_CONNECTORS$AND,
                next_clause=.self$orm__$where_clause(
                    field=.self$orm__$table_field(
                        table=linkage_table_name,
                        field=sprintf("%s_id", other$table__)
                    ),
                    operator=.self$orm__$OPERATORS$EQ,
                    value=other$get_id()
                )
            )
        )
    )
    dbClearResult(.self$orm__$send_statement(request))
    return (request)
})

ModelMeta$methods(create_link_to_=function(other) {
    if (
        (self_id <- .self$get_id()) == -1
        || (other_id <- other$get_id()) == -1
    ) {
        stop("Must save objects in database before linking them")
    }
    if (.self$table__ < other$table__) {
        linkage_table_name <- sprintf(
            "%s_%s", .self$table__, other$table__
        )
    } else {
        linkage_table_name <- sprintf(
            "%s_%s", other$table__, .self$table__
        )
    }
    request <- .self$orm__$create_insert_request(
        table=linkage_table_name,
        fields=list(
            sprintf("%s_id", .self$table__),
            sprintf("%s_id", other$table__)
        ),
        values=list(self_id, other_id)
    )
    dbClearResult(.self$orm__$send_statement(request))
    return (request)
})
