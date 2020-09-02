
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

#' @method as.list ResultSet
#' @export
as.list.ResultSet <- function(x) {
    .self <- selectMethod("$", "envRefClass")(x, ".self")
    return (x$result_set__)
}
setMethod("as.list", "ResultSet", as.list.ResultSet)
setMethod("as.list.default", "ResultSet", as.list.ResultSet)


#' @method as.data.frame ResultSet
#' @export
as.data.frame.ResultSet <- function(x, load_one_to_one=NULL) {
    .self <- selectMethod("$", "envRefClass")(x, ".self")
    if (is.null(first <- .self$first())) {
        return (data.frame())
    }
    field_names <- .self$get_field_names(first, with_foreign=load_one_to_one)
    return (.self$as_given_container(data.frame(), field_names, load_one_to_one))
}
setMethod("as.data.frame", "ResultSet", as.data.frame.ResultSet)
setMethod("as.data.frame.default", "ResultSet", as.data.frame.ResultSet)

#' @method as.matrix ResultSet
#' @export
as.matrix.ResultSet <- function(x, load_one_to_one=NULL) {
    .self <- selectMethod("$", "envRefClass")(x, ".self")
    if (is.null(first <- .self$first())) {
        return (matrix())
    }
    field_names <- .self$get_field_names(first, with_foreign=load_one_to_one)
    result_matrix <- matrix(
        nrow=.self$length(),
        ncol=length(field_names),
        dimnames=list(list(), field_names)
    )
    return (.self$as_given_container(result_matrix, field_names, load_one_to_one))
}
setMethod("as.matrix", "ResultSet", as.matrix.ResultSet)
setMethod("as.matrix.default", "ResultSet", as.matrix.ResultSet)

ResultSet$methods(get_field_names=function(model, with_foreign=NULL) {
    orm <- model$orm__
    field_names <- names(model$fields__)
    if (!is.null(with_foreign)) {
        models <- orm$model_definitions_
        for (table in with_foreign) {
            sub_fields <- names(models[[table]]$fields)
            field_names <- c(
                field_names,
                map(sub_fields, function(field) {
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

