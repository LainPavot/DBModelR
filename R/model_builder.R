

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
        return ("blob")
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

generate_methods_ <- function(model, class_name, fields) {
    return (generate_setters_getters_(model, class_name, fields))
}

generate_setters_getters_ <- function(model, class_name, fields) {
    methods <- list()
    for (field in names(fields)) {
        if (any(grepl("^.*_id$", field))) {
            table <- sub("_id$", "", field)
            if (any(grepl(sprintf("^%s$", table), model$one))) {
                getter <- generate_fk_getter_(table, field)
                setter <- generate_fk_setter_(class_name, table)
                field <- table
            }
        } else {
            getter <- generate_getter_(field)
            setter <- generate_setter_(class_name, field, fields[[field]])
        }
        methods[[sprintf("get_%s", field)]] <- getter
        methods[[sprintf("set_%s", field)]] <- setter
    }
    for (table in model$many) {
        methods[[sprintf("get_%s", table)]] <- (
            generate_get_many(model, table)
        )
        methods[[sprintf("add_%s", table)]] <- (
            generate_add_many(model, table)
        )
        methods[[sprintf("set_%s", table)]] <- (
            generate_set_many(model, table)
        )
        methods[[sprintf("remove_%s", table)]] <- (
            generate_remove_many(model, table)
        )
    }
    return (methods)
}

generate_fk_setter_ <- function(class_name, table_name) {
    setter <- function(value) {
        field <- field
        .self$modified__[[field]] <- value
    }
    field <- sprintf("%s_id", table_name)
    setter <- inject_local_function_dependencies_(setter, 2, field)
    return (setter)
}

generate_fk_getter_ <- function(table_name, field) {
    getter <- function() {
        table_name <- table_name
        field <- field
        no_cache <- TRUE
        if (is.null(.self$cache_read__[[table_name]]) || no_cache) {
            .self$cache_read__[[table_name]] <- (
                .self$orm__$model_objects_[[
                    table_name
                ]]()$load(.self[[field]])
            )
        }
        return (.self$cache_read__[[table_name]])
    }
    getter <- inject_local_function_dependencies_(
        getter, 2, table_name, field
    )
    return (getter)
}

generate_getter_ <- function(field) {
    getter <- function() {
        field <- field
        return (.self[[field]])
    }
    ## crapy hack to pass the value of "field" to the function...
    getter <- inject_local_function_dependencies_(getter, 2, field)
    return (getter)
}

generate_setter_ <- function(class_name, field, type) {
    if (type == "numeric") {
        test <- is.numeric
    } else if (type == "character") {
        test <- is.character
    } else if (type == "blob") {
        test <- function(x)is(x, "blob")
    } else {
        print(sprintf("Unknown filed type: %s", type))
    }
    err_string <- sprintf(
        "Bad field type for %s$%s.
        Type <%s> expected, got <%%s> with value: %%s",
        class_name, field, type
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
    setter <- inject_local_function_dependencies_(
        setter, 2, field, err_string, test
    )
    return (setter)
}

generate_get_many <- function(schema, other_table) {
    getter <- function(...) {
        linkage_table_name <- linkage_table_name
        other_table <- other_table
        no_cache <- length(list(...)) != 0
        no_cache <- TRUE
        if (is.null(.self$cache_read__[[other_table]]) || no_cache) {
            other <- .self$orm__$model_objects_[[other_table]]()
            result <- (
                other$load_by(
                    distinct=TRUE,
                    add_from=list(.self$table__),
                    other$join_clause(
                        table=linkage_table_name,
                        on=.self$operator_clause(
                            left=other$table_field("id"),
                            right=.self$orm__$table_field(
                                table=linkage_table_name,
                                field=sprintf("%s_id", other$table__)
                            ),
                            operator=.self$orm__$OPERATORS$EQ
                        ),
                        as_right="link_1"
                    ),
                    other$join_clause(
                        table=linkage_table_name,
                        on=.self$operator_clause(
                            left=.self$table_field("id"),
                            right=.self$orm__$table_field(
                                table="link_2",
                                field=sprintf("%s_id", .self$table__)
                            ),
                            operator=.self$orm__$OPERATORS$EQ
                        ),
                        as_right="link_2"
                    ),
                    ...
                )
            )
            if (!is.list(result)) {
                result <- list(result)
            }
            if (no_cache) {
                return (result)
            }
            .self$cache_read__[[other_table]] <- result
        }
        return (.self$cache_read__[[other_table]])
    }
    if (schema$table < other_table) {
        linkage_table_name <- sprintf("%s_%s", schema$table, other_table)
    } else {
        linkage_table_name <- sprintf("%s_%s", other_table, schema$table)
    }
    getter <- inject_local_function_dependencies_(
        getter, 2,
        linkage_table_name, other_table
    )
    return (getter)
}

generate_add_many <- function(schema, other_table) {
    adder <- function(value) {
        target_class <- target_class
        if (!is.list(value)) {
            value <- list(value)
        }
        if (is.null(.self$cache_add__[[value[[1]]$table__]])) {
            .self$cache_add__[[value[[1]]$table__]] <- list()
        }
        for (obj in value) {
            if (!is(obj, target_class)) {
                stop(sprintf(
                    "Trying to add a %s that is not a Model to a %s",
                    target_class, class(.self)
                ))
            }
            .self$cache_add__[[obj$table__]][[
                length(.self$cache_add__[[obj$table__]])+1
            ]] <- obj
        }
        return (.self)
    }
    adder <- inject_local_function_dependencies_(
        adder, 2, class_name_from_table_name_(other_table)
    )
    return (adder)
}

generate_set_many <- function(schema, other_table) {
    setter <- function(value) {
        target_class <- target_class
        if (!is.list(value)) {
            value <- list(value)
        } 
        .self$cache_add__[[value[[1]]$table__]] <- list()
        for (obj in value) {
            if (!is(obj, target_class)) {
                stop(sprintf(
                    "Trying to add a %s that is not a Model to a %s",
                    target_class, class(.self)
                ))
            }
            .self$cache_add__[[obj$table__]][[
                length(.self$cache_add__[[obj$table__]])+1
            ]] <- obj
        }
        return (.self)
    }
    setter <- inject_local_function_dependencies_(
        setter, 2, class_name_from_table_name_(other_table)
    )
    return (setter)
}

generate_remove_many <- function(schema, other_table) {
    remover <- function(value) {
        target_class <- target_class
        if (!is.list(value)) {
            value <- list(value)
        } 
        if (is.null(.self$cache_remove__[[value[[1]]$table__]])) {
            .self$cache_remove__[[value[[1]]$table__]] <- list()
        }
        for (obj in value) {
            if (!is(obj, target_class)) {
                stop(sprintf(
                    "Trying to add a %s that is not a Model to a %s",
                    target_class, class(.self)
                ))
            }
            .self$cache_remove__[[obj$table__]][[
                length(.self$cache_remove__[[obj$table__]])+1
            ]] <- obj
        }
        return (.self)
    }
    remover <- inject_local_function_dependencies_(
        remover, 2, class_name_from_table_name_(other_table)
    )
    return (remover)
}

class_name_from_table_name_ <- function(table_name) {
    return (paste(
        toupper(substring(table_name, 1, 1)),
        substring(table_name, 2),
        "Model", sep=""
    ))
}

inject_local_function_dependencies_ <- function(func, begin, ...) {
    "
    crapy hacks to pass the values to the function...
    otherwise the function loses it's env and refs to these vars
    "
    dependencies <- list(...)
    position <- begin
    for (value in dependencies) {
        body(func)[[position]][[3]] <- value
        position <- position+1
    }
    return (func)
}

#' Create a model class from a model definition
#'
#' @param model A `ModelDefinition` object.
#' @param orm An `ORM` object.
#' @param additional_fields A `list` to define additionnal fields
#' @param ... Any args passer to setRefClass
#' @return The generator of the class created from the \code{model}
model_builder <- function(model, orm, additional_fields=list(), ...) {
    class_name <- class_name_from_table_name_(model$table)
    fields <- sqlite_fields_to_R_types_(model$fields)
    methods <- generate_methods_(model, class_name, fields)
    methods[["initialize"]] <- function(...) {
        callSuper(...)
        .self$table__ <- table
        .self$sql_model__ <- model
        .self$model_name__ <- class_name
        .self$orm__ <- orm
        .self$fields__ <- .self$sql_model__$fields
        params <- list(...)
        .self$id <- -1
        for(field in names(.self$fields__)) {
            if (field != "id") {
                if (!is.null(params[[field]])) {
                    .self$modified__[[field]] <- .self[[field]]
                    .self[[field]] <- params[[field]]
                } else {
                    type <- .self$fields__[[field]]
                    if (type == "TEXT") {
                        .self[[field]] <- ""
                    } else if (type == "BLOB") {
                        .self[[field]] <- blob::blob()
                    } else if (type == "BOOLEAN") {
                        .self[[field]] <- FALSE
                    } else if (
                        type == "INTEGER"
                        || type == "FLOAT"
                        || type == "REAL"
                    ) {
                        .self[[field]] <- 0
                    } else {
                        stop(sprintf("Unknown field type: %s", type))
                    }
                }
            }
        }
    }
    methods$initialize <- inject_local_function_dependencies_(
        methods$initialize, 3,
        model$table, model, class_name, orm
    )
    # position <- 3
    # for (value in list(model$table, model, class_name, orm)) {
    #     body(methods$initialize)[[position]][[3]] <- value
    #     position <- position+1
    # }
    generator <- setRefClass(
        class_name,
        fields=c(fields, additional_fields),
        methods=methods,
        contains="ModelMeta",
        where=.GlobalEnv,
        ...
    )
    do.call(generator$methods, methods)
    return (generator)
}
