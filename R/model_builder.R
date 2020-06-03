

sqlite_field_to_R_type_ <- function(field) {
    if (identical(field, "INTEGER")) {
        return ("numeric")
    } else if (identical(field, "TEXT")) {
        return ("character")
    } else if (identical(field, "FLOAT")) {
        return ("numeric")
    } else if (identical(field, "REAL")) {
        return ("numeric")
    } else if (identical(field, "BLOB")) {
        return ("character")
    }
    ## todo: throw error
    return ("unknown")
}

sqlite_fields_to_R_types_ <- function(list) {
    for (i in seq_along(list)) {
        list[[i]] <- sqlite_field_to_R_type_(list[[i]])
    }
    return (list)
}

generate_methods_ <- function(class_name, fields) {
    return (generate_setters_getters_(class_name, fields))
}

generate_setters_getters_ <- function(class_name, fields) {
    methods <- list()
    for (field in names(fields)) {
        methods[[paste("get_", field, sep="")]] <- generate_getter_(field)
        methods[[paste("set_", field, sep="")]] <- generate_setter_(class_name, field, fields[[field]])
    }
    methods[["get_id"]] <- generate_getter_("id")
    methods[["set_id"]] <- generate_setter_(class_name, "id", "numeric")
    return (methods)
}

generate_getter_ <- function(field) {
    setter <- function().self[[field]]
    ## crapy hack to pass the value of "field" to the function...
    body(setter)[[2]] <- field
    return (setter)
}

generate_setter_ <- function(class_name, field, type) {
    if (type == "numeric") {
        test <- is.numeric
    } else if (type == "character") {
        test <- is.character
    }
    err_string <- paste(
        "Bad field type for ", class_name, "$", field, ". Type <",
        type, "> expected, got <%s> with value: %s", sep=""
    )
    setter <- function(value) {
        field <- field
        err_string <- err_string
        test <- test
        if (missing(value)) {
            return(.self$field(field, value))
        } else if (!test(value)) {
            stop(sprintf(err_string, class(value), value))
        }
        if (.self[[field]] == value) {
            return (.self)
        }
        if (is.null(.self$modified__[[field]])) {
            ## if not modified yet, add a "modified" flag for this field
            if (.self$loaded__) {
                .self$modified__[[field]] <- .self[[field]]
            } else {
                .self$modified__[[field]] <- value
            }
        } else if (.self$modified__[[field]] == value) {
            ## if already modified and back to ariginal value,
            ## remove the "modified" flag for this field
            .self$modified__[[field]] <- NULL
        }
        .self[[field]] <- value
        return (.self)
    }
    ## crapy hacks to pass the values of "field"; "err_string" and
    ## "test" to the function...
    ## otherwise the function loses it's env and refs to these vars
    position <- 2
    for (value in c(field, err_string, test)) {
        body(setter)[[position]][[3]] <- value
        position <- position + 1
    }
    return (setter)
}

#' Create a model class from a model definition
#'
#' @param model A `ModelDefinition` object.
#' @param orm An `ORM` object.
#' @author Lain Pavot
model_builder <- function(model, orm, additional_fields=list(), ...) {
    class_name <- paste(
        toupper(substring(model$table, 1, 1)),
        substring(model$table, 2),
        "Model", sep=""
    )
    fields <- sqlite_fields_to_R_types_(model$fields)
    methods <- generate_methods_(class_name, fields)
    methods[["initialize"]] <- function(...) {
        callSuper(...)
        .self$table__ <- table
        .self$sql_model__ <- sql_model
        .self$model_name__ <- model_name
        .self$orm__ <- orm
        .self$fields__ <- .self$sql_model__$fields
        params <- list(...)
        for(field in names(.self$fields__)) {
            if (!is.null(params[[field]]) || field == "id") {
                .self$modified__[[field]] <- .self[[field]]
                .self[[field]] <- params[[field]]
            } else {
                type <- .self$fields__[[field]]
                if ((type == "TEXT") || (type == "BLOB")) {
                    .self[[field]] <- ""
                } else if (type == "BOOLEAN") {
                    .self[[field]] <- FALSE
                } else if (type == "INTEGER" || type == "FLOAT" || type == "REAL") {
                    .self[[field]] <- 0
                } else {
                    stop(sprintf("Unknown field type: %s", type))
                }
            }
        }
    }
    position <- 3
    for (value in list(model$table, model, class_name, orm)) {
        body(methods$initialize)[[position]][[3]] <- value
        position <- position+1
    }
    generator <- setRefClass(
        class_name,
        fields=c(fields, additional_fields),
        methods=methods,
        contains="ModelMeta",
        ...
    )
    do.call(generator$methods, methods)
    return (generator)
}
