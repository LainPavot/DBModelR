
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
    .self$NOT_CREATED <- -1
    .self$NOT_RETRIEVED <- -2
})

ModelMeta$methods(clear=function(...) {
    for (field in names(.self$fields__)) {
        type <- .self$fields__[[field]]
        if (type == "TEXT") {
            .self[[field]] <- ""
        } else if (
            type == "INTEGER"
            || type == "FLOAT"
            || type == "REAL"
        ) {
            .self[[field]] <- 0
        } else if (type == "BOOLEAN") {
            .self[[field]] <- FALSE
        } else if (type == "BLOB") {
            .self[[field]] <- blob::blob(raw())
        }
    }
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
    cat(.self$as.character())
})


ModelMeta$methods(as.character=function() {
    "\
    "
    return (paste(
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


ModelMeta$methods(created=function(){
    "\
    "
    return (.self$id != .self$NOT_CREATED)
})


ModelMeta$methods(all=function(){
    "\
    "
    return(.self$load_by())
})


ModelMeta$methods(load=function(id){
    "\
    "
    result <- .self$load_by(id=id)
    if (result$length() == 0) {
        return (NULL)
    }
    return (result$first())
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
                if (is.list(value)) {
                    operator <- .self$orm__$OPERATORS$IN
                } else if (is.vector(value) && length(value) > 1) {
                    operator <- .self$orm__$OPERATORS$IN
                    value <- as.list(value)
                } else {
                    operator <- .self$orm__$OPERATORS$EQ
                }
                where[[length(where)+1]] <- list(
                    field=.self$table_field(field=field),
                    value=value,
                    operator=operator
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

ModelMeta$methods(load_from_request__=function(request) {
    "\
    "
    rs <- .self$orm__$get_query(request)
    if (nrow(rs) == 0) {
        return (ResultSet())
    }
    return (ResultSet(results=.self$load_multiple_from_data__(rs)))
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


ModelMeta$methods(save=function(bulk=list(), return_request=FALSE) {
    "\
    "
    ## bulk is not used for the moment ; too complicated, and
    ## perhaps useless in XSeeker:
    ## it saves in a pool the models to save and when the user
    ## decides to save them, they're all saved with only one request
    orm <- .self$orm__
    if (identical(bulk, list())) {
        if (!.self$created()) {
            .self$save_added_fk_()
            field_names <- as.list(names(.self$fields__))
            field_names[field_names=="id"] <- NULL
            request <- orm$create_insert_request(
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
                field_names <- as.list(names(.self$modified__))
                field_names[field_names=="id"] <- NULL
                for (field in field_names) {
                    values[[field]] <- .self[[field]]
                }
                request <- orm$create_update_request(
                    table=.self$table__,
                    values=values,
                    where=list(
                        .self$where_clause(
                            field=.self$table_field(field="id"),
                            value=.self$get_id(),
                            operator=orm$OPERATORS$EQ
                        )
                    )
                )
            }
            .self$save_added_fk_()
            .self$save_removed_many_()
            new_row <- FALSE
        }
    } else {
        return (.self$bulk_save(bulk))
    }
    if (request != "") {
        get_id <- (
            if (new_row) "SELECT last_insert_rowid() as id"
            else sprintf("SELECT %s as id", orm$escape(.self$get_id()))
        )
        orm$with_atomic(
            before={
                orm$clear_result(orm$send_statement(request))
                orm$get_query(get_id)
            },
            then={
                context <- orm$execution_context
                .self$set_id(context$rs$id)
                .self$modified__ <- list()
            }
        )
    }
    ## once the object is updated/created, we update its links
    .self$save_added_many_()
    return (if (return_request) request else .self)
})

ModelMeta$methods(bulk_save=function(models) {
    to_insert <- list()
    self_type <- class(.self)

    field_names <- as.list(names(.self$fields__))
    field_names[field_names=="id"] <- NULL
    for (model in models) {
        if (is.list(model)) {
            values <- list()
            for (field in field_names) {
                # value <- model[[field]]
                if(is.null(value <- model[[field]])) {
                    value <- do.call(.self$fields__[[field]], c())
                }
                values[[field]] <- value
            }
            to_insert[[length(to_insert)+1]] <- values
        } else if (!is(model, self_type)) {
            stop(sprintf(
                "%s: Bulk save only work with similar objects, %s found.",
                self_type, class(model)
            ))
        } else if (model$created()) {
            model$save()
        } else {
            model$save_added_fk_()
            values <- list()
            for (field in field_names) {
                values[[field]] <- .self[[field]]
            }
            to_insert[[length(to_insert)+1]] <- values
            model$id <- model$NOT_RETRIEVED
        }
    }
    if (length(to_insert)) {
        print("Bukl request created")
        request <- .self$orm__$create_insert_request(
            table=.self$table__,
            fields=field_names,
            values=to_insert
        )
        .self$orm__$clear_result(.self$orm__$send_statement(request))
    }
    # stop(sprintf(
    #     "%s$save(bulk=list(<%s>)) is not implemented yet."
    #     , model_name__, model_name__
    # ))
})

ModelMeta$methods(save_added_fk_=function() {
    for (obj in .self$modified__) {
        if (is(obj, "ModelMeta") && !obj$created()) {
            obj$save()
            .self$cache_read__[[obj$table__]] <- NULL
        }
    }
})

ModelMeta$methods(save_added_many_=function() {
    for (table in names(.self$cache_add__)) {
        for (obj in .self$cache_add__[[table]]) {
            if (!obj$created()) {
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
    .self$orm__$clear_result(.self$orm__$send_statement(request))
    return (request)
})

ModelMeta$methods(create_link_to_=function(other) {
    if (
        (self_id <- .self$get_id()) == .self$NOT_CREATED
        || (other_id <- other$get_id()) == .self$NOT_CREATED
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
    .self$orm__$clear_result(.self$orm__$send_statement(request))
    return (request)
})
