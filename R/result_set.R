
#' Returns ModelMeta at the given index.
#' @param x A ResultSet instance
#' @param i The index of the ModelMeta
#' @return The ModelMeta instance of index \code{index}
setMethod("[[", "ResultSet", function(x, i) {
    .self <- selectMethod("$", "envRefClass")(x, ".self")
    return (.self$result_set__[[i]])
})

#' Returns the number of results.
#' @param x A ResultSet instance
#' @return The number of results
setMethod("length", "ResultSet", function(x) {
    .self <- selectMethod("$", "envRefClass")(x, ".self")
    return (.self$length__)
})

setMethod("as.vector", "ResultSet", function(x) {
    .self <- selectMethod("$", "envRefClass")(x, ".self")
    return (as.vector(.self$result_set__))
})

#' @export
as.list.ResultSet <- function(x) {
    .self <- selectMethod("$", "envRefClass")(x, ".self")
    return (x$result_set__)
}
setMethod("as.list", "ResultSet", as.list.ResultSet)
setMethod("as.list.default", "ResultSet", as.list.ResultSet)

#' @export
as.matrix.ResultSet <- function(x, load_one_to_one=NULL) {
    .self <- selectMethod("$", "envRefClass")(x, ".self")
    if (is.null(first <- .self$first())) {
        return (NA)
    }
    orm <- first$orm__
    field_names <- names(first$fields__)
    if (!is.null(load_one_to_one)) {
        models <- orm$model_definitions_
        for (table in load_one_to_one) {
            sub_fields <- names(models[[table]]$fields)
            field_names <- c(
                field_names,
                map(sub_fields, function(field) {
                    return (sprintf("%s_%s", table, field))
                })
            )
        }
    }
    result_matrix <- matrix(
        nrow=.self$length(),
        ncol=length(field_names),
        dimnames=list(list(), field_names)
    )
    i <- 1
    for (result in as.vector(.self)) {
        for (field_name in names(first$fields__)) {
            result_matrix[i, field_name] <- result[[field_name]]
        }
        if (!is.null(load_one_to_one)) {
            for (table in load_one_to_one) {
                if (any(grepl(sprintf("^%s$", table), first$sql_model__$one))) {
                    foreign_object <- orm$model_objects_[[table]]()$load(
                        result[[sprintf("%s_id", table)]]
                    )
                } else {
                    args <- list()
                    args[[sprintf("%s_id", first$table__)]] <- result$get_id()
                    foreign_object_rs <- do.call(
                        orm$model_objects_[[table]]()$load_by, args
                    )
                    if (length(foreign_object_rs) != 1) {
                        next
                    }
                    foreign_object <- foreign_object_rs$first()
                }
                for (field_name in names(models[[table]]$fields)) {
                    result_matrix[
                        i, sprintf("%s_%s", table, field_name)
                    ] <- foreign_object[[field_name]]
                }
            }
        }
        i <- i + 1
    }
    return (result_matrix)
}
setMethod("as.matrix", "ResultSet", as.matrix.ResultSet)
setMethod("as.matrix.default", "ResultSet", as.matrix.ResultSet)

ResultSet$methods(initialize=function(results=NULL) {
    .self$result_set__ <- (if (is.null(results)) list() else results)
    .self$length__ <- base::length(.self$result_set__)
})

ResultSet$methods(length=function() {
    return (.self$length__)
})

ResultSet$methods(first=function() {
    if (.self$length__ == 0) {
        return (NULL)
    }
    return (.self$result_set__[[1]])
})

ResultSet$methods(last=function() {
    if (.self$length__ == 0) {
        return (NULL)
    }
    return (.self$result_set__[[.self$length__]])
})

ResultSet$methods(any=function() {
    return (.self$length__ != 0)
})

ResultSet$methods(delete=function() {
    if (!is.null(first_model <- .self$first())) {
        orm <- first_model$orm__
        orm$execute(orm$create_delete_request(
            table=first_model$table__,
            where=list(
                orm$where_clause(
                    field=first_model$table_field("id"),
                    operator=orm$OPERATORS$IN,
                    value=map(.self$result_set__, function(x) x$get_id())
                )
            )
        ))
    }
})

ResultSet$methods(fields=function(field) {
    return (map(
        .self$result_set__,
        function(x)x[[field]]
    ))
})