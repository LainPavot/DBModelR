
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

#' @method as.matrix ModelMeta
#' @export
as.matrix.ModelMeta <- function(x, load_one_to_one=NULL) {
    .self <- selectMethod("$", "envRefClass")(x, ".self")
    orm <- .self$orm__
    field_names <- names(.self$fields__)
    if (!is.null(load_one_to_one)) {
        models <- orm$model_definitions_
        for (table in load_one_to_one) {
            sub_fields <- names(models[[table]]$fields)
            field_names <- c(
                field_names,
                mapply(function(field) {
                    return (sprintf("%s_%s", table, field))
                }, sub_fields, SIMPLIFY=FALSE)
            )
        }
    }
    return (.self$as_matrix_internal(
        field_names, load_one_to_one
    ))
}
setMethod("as.matrix", "ModelMeta", as.matrix.ModelMeta)
setMethod("as.matrix.default", "ModelMeta", as.matrix.ModelMeta)

ModelMeta$methods(as_matrix_internal=function(field_names, load_one_to_one, orm) {
    return (.self$as_given_container(
        matrix(
            nrow=1,
            ncol=length(field_names),
            dimnames=list(list(1), field_names)
        ),
        load_one_to_one, 1
    ))
})

#' @method as.data.frame ModelMeta
#' @export
as.data.frame.ModelMeta <- function(x, load_one_to_one=NULL) {
    .self <- selectMethod("$", "envRefClass")(x, ".self")
    return (.self$as_data.frame_internal(load_one_to_one))
}
setMethod("as.data.frame", "ModelMeta", as.data.frame.ModelMeta)
setMethod("as.data.frame.default", "ModelMeta", as.data.frame.ModelMeta)

ModelMeta$methods(as_data.frame_internal=function(load_one_to_one) {
    return (.self$as_given_container(
        data.frame(),
        load_one_to_one,
        1
    ))
})

ModelMeta$methods(as_given_container=function(container, load_one_to_one, index) {
    orm <- .self$orm__
    models <- orm$model_definitions_
    for (field_name in names(.self$fields__)) {
        container[index, field_name] <- .self[[field_name]]
    }
    if (!is.null(load_one_to_one)) {
        for (table in load_one_to_one) {
            if (any(grepl(sprintf("^%s$", table), .self$sql_model__$one))) {
                foreign_object <- orm$model_objects_[[table]]()$load(
                    .self[[sprintf("%s_id", table)]]
                )
            } else {
                args <- list()
                args[[sprintf("%s_id", .self$table__)]] <- .self$get_id()
                foreign_object_rs <- do.call(
                    orm$model_objects_[[table]]()$load_by, args
                )
                if (length(foreign_object_rs) != 1) {
                    for (field_name in names(models[[table]]$fields)) {
                        container[
                            index, sprintf("%s_%s", table, field_name)
                        ] <- NA
                    }
                    next
                }
                foreign_object <- foreign_object_rs$first()
            }
            for (field_name in names(models[[table]]$fields)) {
                container[
                    index, sprintf("%s_%s", table, field_name)
                ] <- foreign_object[[field_name]]
            }
        }
    }
    return (container)
})


#' @method as.list ModelMeta
#' @export
as.list.ModelMeta <- function(x, fields=NULL) {
    .self <- selectMethod("$", "envRefClass")(x, ".self")
    if (is.null(fields)) {
        fields <- names(.self$fields__)
    }
    result <- lapply(fields, function(x){return (.self[[x]])})
    names(result) <- fields
    return (result)
}
setMethod("as.list", "ModelMeta", as.list.ModelMeta)
setMethod("as.list.default", "ModelMeta", as.list.ModelMeta)

ModelMeta$methods(initialize=function(...) {
    "\
    "
    callSuper(...)
    .self$modified__ <- list()
    .self$table__ <- "meta"
    .self$fields__ <- list()
    .self$model_name__ <- "ModelMeta"
    .self$orm__ <- DBModelR::ORM()
    .self$loaded__ <- FALSE
    .self$cache_read__ <- list()
    .self$cache_remove__ <- list()
    .self$cache_add__ <- list()
    .self$sql_model__ <- DBModelR::ModelDefinition(
        table=.self$table__,
        fields=.self$fields__
    )
    .self$NOT_CREATED <- -1
    .self$NOT_RETRIEVED <- -2
})

ModelMeta$methods(clear=function(..., unset_id=FALSE) {
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
    if (unset_id) {
        .self$set_id(.self$NOT_CREATED)
    }
    .self$modified__ <- list()
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
        paste(mapply(
            function(field) {
                value <- .self[[field]]
                if (is.character(value)) {
                    fmt <- "[%s: \"%s\"]"
                } else {
                    fmt <- "[%s: %s]"
                }
                return (sprintf(fmt, field, value))
            },
            names(.self$fields__)[names(.self$fields__) != "id"],
            SIMPLIFY=FALSE
        ), collapse="\n  "),
        "", sep="\n  "
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
            next
        }
        if (!is.null(field) && field == "add_from") {
            add_from[[length(add_from)+1]] <- value
            next
        }
        if (!is.null(field) && field == "distinct") {
            distinct <- value
            next
        }
        if (previous_is_where) {
            where[[length(where)+1]] <- .self$orm__$LOGICAL_CONNECTORS$AND
        }
        if (is(value, "expression")) {
            where_clause <- .self$unparse_where_expression(field, value)
        } else {
            where_clause <- .self$create_where_clause(field, value)
        }
        where[[length(where)+1]] <- where_clause
        previous_is_where <- TRUE
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

ModelMeta$methods(create_where_clause=function(field, value) {
    if (is(value, "WhereClause")) {
        return (value)
    }
    if (is(value, "expression") || is(value, "call")) {
         return (.self$unparse_where_expression(field, value))
    }
    if (is(value, "ModelMeta")) {
        if (id <- value$get_id() == -1) {
            stop(sprintf(paste0(
                "The %s has never been saved to the ",
                "database, and so has no id. You must ",
                "save the it before."
            )), class(value))
        }
        value <- id
    }
    return (.self$create_where_clause_from_raw_value(field, value))
})

ModelMeta$methods(create_where_clause_from_raw_value=function(field, value, operator=NULL) {
    if (is.null(.self$fields__[[field]])) {
        stop(sprintf(
            "Field <%s> does not exists for table <%s>",
            field, .self$table__
        ))
    }
    if (identical(operator, NULL)) {
        if (is.list(value)) {
            operator <- .self$orm__$OPERATORS$IN
        } else if (is.vector(value) && length(value) > 1) {
            operator <- .self$orm__$OPERATORS$IN
            value <- as.list(value)
        } else {
            operator <- .self$orm__$OPERATORS$EQ
        }
    } else {
        if (is.list(value)) {
            if (operator == .self$orm__$OPERATORS$NE) {
                operator <- .self$orm__$OPERATORS$NIN
            } else if (operator == .self$orm__$OPERATORS$LIKE) {
                stop("Cannot use operator LIKE with a list of values")
            }
        }
    }
    return (list(
        field=.self$table_field(field=field),
        value=value,
        operator=operator
    ))
})

ModelMeta$methods(unparse_where_expression=function(field, value) {
    if (is(value, "call")) {
        value <- as.expression(value)
    }
    exprstring <- value[[1]]
    first <- deparse(exprstring[[1]])
    if (length(exprstring) == 2) {
        if (first == "!") {
            if (length(exprstring[[2]]) == 2 && deparse(exprstring[[2]][[1]]) == "~") {
                operator <- .self$orm__$OPERATORS$NLIKE
                value <- exprstring[[2]][[2]]
            } else {
                operator <- .self$orm__$OPERATORS$NE
                value <- exprstring[[2]]
            }
        } else if (first == "~") {
            operator <- .self$orm__$OPERATORS$LIKE
            value <- exprstring[[2]]
        } else {
            stop(sprintf("Bad operator: %s", first))
        }
    } else if (length(exprstring) == 3) {
        if (first == "|") {
            operator <- gsub("'", "", gsub("\"", "", exprstring[[2]]))
        } else {
            stop(sprintf("Bad operator: %s", first))
        }
        value <- exprstring[[3]]
    } else {
        stop("Malformed operator: too many arguments (2 or 3 expected): %s", exprstring)
    }
    if (is(value, "call")) {
        value <- eval(value)
    }
    return (.self$create_where_clause_from_raw_value(
        field, value, operator=operator
    ))
})

ModelMeta$methods(load_from_request__=function(request) {
    "\
    "
    rs <- .self$orm__$get_query(request)
    if (nrow(rs) == 0) {
        return (DBModelR::ResultSet())
    }
    return (DBModelR::ResultSet(results=.self$load_multiple_from_data__(rs)))
})


ModelMeta$methods(load_multiple_from_data__=function(multiple) {
    "\
    "
    generator <- .self$getRefClass()
    return (mapply(
        function(row) {
            generator()$load_one_from_data__(multiple[row,])
        },
        seq_len(nrow(multiple)),
        SIMPLIFY=FALSE
    ))
})


ModelMeta$methods(load_one_from_data__=function(row) {
    "\
    "
    for (field in names(row)) {
        if (.self$fields__[[field]] == "BLOB") {
            field <- field[[1]]
        }
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
                values=mapply(function(x).self[[x]], field_names, SIMPLIFY=FALSE)
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
            for (field in field_names) {
                if(is.null(model[[field]])) {
                    model[[field]] <- do.call(.self$fields__[[field]])
                }
            }
            to_insert[[length(to_insert)+1]] <- model
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
        request <- .self$orm__$create_insert_request(
            table=.self$table__,
            fields=field_names,
            values=to_insert
        )
        .self$orm__$clear_result(.self$orm__$send_statement(request))
    }
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
    .self$orm__$execute(request)
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
