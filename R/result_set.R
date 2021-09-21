
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

#' @method as.vector ResultSet
#' @param x A ResultSet instance
#' @export
as.vector.ResultSet <- function(x) {
    .self <- selectMethod("$", "envRefClass")(x, ".self")
    return (as.vector(.self$result_set__))
}
setMethod("as.vector", "ResultSet", as.vector.ResultSet)

#' @method as.character ResultSet
#' @param x A ResultSet instance
#' @export
as.character.ResultSet <- function(x) {
    .self <- selectMethod("$", "envRefClass")(x, ".self")
    return (as.character(.self$result_set__))
}
setMethod("as.character", "ResultSet", as.character.ResultSet)
invisible(setMethod("as.character.default", "ResultSet", as.character.ResultSet))

#' @method print ResultSet
#' @param x A ResultSet instance
#' @export
print.ResultSet <- function(x) {
    .self <- selectMethod("$", "envRefClass")(x, ".self")
    return (print(.self$result_set__))
}
setMethod("print", "ResultSet", print.ResultSet)
invisible(setMethod("print.default", "ResultSet", print.ResultSet))


#' @method as.list ResultSet
#' @param x A ResultSet instance
#' @export
as.list.ResultSet <- function(x, ...) {
    .self <- selectMethod("$", "envRefClass")(x, ".self")
    return (.self$result_set__)
}
setMethod("as.list", "ResultSet", as.list.ResultSet)
invisible(setMethod("as.list.default", "ResultSet", as.list.ResultSet))


#' @method as.data.frame ResultSet
#' @param x A ResultSet instance
#' @export
as.data.frame.ResultSet <- function(
    x,
    row.names=NULL,
    optional=NULL,
    load_one_to_one=NULL,
    ...
) {
    .self <- selectMethod("$", "envRefClass")(x, ".self")
    if (is.null(first <- .self$first())) {
        return (data.frame())
    }
    field_names <- .self$get_field_names(first, with_foreign=load_one_to_one)
    return (.self$as_given_container(data.frame(), field_names, load_one_to_one))
}
setMethod("as.data.frame", "ResultSet", as.data.frame.ResultSet)
invisible(setMethod("as.data.frame.default", "ResultSet", as.data.frame.ResultSet))

#' @method as.matrix ResultSet
#' @param x A ResultSet instance
#' @export
as.matrix.ResultSet <- function(x, load_one_to_one=NULL, ...) {
    .self <- selectMethod("$", "envRefClass")(x, ".self")
    if (!.self$df_built__) {
        if (is.null(first <- .self$first())) {
            return (matrix())
        }
        field_names <- .self$get_field_names(first, with_foreign=load_one_to_one)
        result_matrix <- matrix(
            nrow=.self$length(),
            ncol=length(field_names),
            dimnames=list(list(), field_names)
        )
        .self$df_cache__ <- .self$as_given_container(
            result_matrix,
            field_names,
            load_one_to_one
        )
        .self$df_built__ <- TRUE
    }
    return (.self$df_cache__)
}
setMethod("as.matrix", "ResultSet", as.matrix.ResultSet)
invisible(setMethod("as.matrix.default", "ResultSet", as.matrix.ResultSet))

ResultSet$methods(get_field_names=function(model, with_foreign=NULL) {
    orm <- model$orm__
    field_names <- names(model$fields__)
    if (!is.null(with_foreign)) {
        models <- orm$model_definitions_
        for (table in with_foreign) {
            sub_fields <- names(models[[table]]$fields)
            field_names <- c(
                field_names,
                lapply(sub_fields, function(field) {
                    return (sprintf("%s_%s", table, field))
                })
            )
        }
    }
    return (field_names)
})

ResultSet$methods(as_given_container=function(
    container, field_names, load_one_to_one
) {
    i <- 1
    for (result in as.vector(.self)) {
        container <- result$as_given_container(container, load_one_to_one, i)
        i <- i + 1
    }
    return (container)
})

ResultSet$methods(initialize=function(results=NULL, original_df=NULL) {
    .self$result_set__ <- (if (is.null(results)) list() else results)
    .self$length__ <- base::length(.self$result_set__)
    .self$df_built__ <- !is.null(original_df)
    if (.self$df_built__) {
        .self$df_cache__ <- original_df
    }
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
                    value=lapply(.self$result_set__, function(x) x$get_id())
                )
            )
        ))
    }
})

ResultSet$methods(fields=function(..., multi_dim_type=data.frame) {
    fields <- list(...)
    if (base::length(fields) == 1) {
        return (.self$field(fields[[1]]))
    }
    result <- multi_dim_type()
    if (base::length(fields) > 1) {
        for (i in seq_along(fields)) {
            result[i,] <- .self$fields(fields[[i]])
        }
    }
    return (result)
})

ResultSet$methods(field=function(asked_field, SIMPLIFY=FALSE, ...) {
    return (mapply(
        function(x)x[[asked_field]],
        .self$result_set__,
        SIMPLIFY=SIMPLIFY,
        ...
    ))
})

ResultSet$methods(max=function(field) {
    return (do.call(base::max, .self$field(field)))
})

ResultSet$methods(min=function(field) {
    return (do.call(base::min, .self$field(field)))
})

ResultSet$methods(mean=function(field) {
    return (base::mean(.self$field(field, SIMPLIFY=TRUE)))
})

ResultSet$methods(median=function(field) {
    return (stats::median(.self$field(field, SIMPLIFY=TRUE)))
})
