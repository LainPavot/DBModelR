

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

generate_methods_ <- function(model, class_name, fields) {
    return (generate_setters_getters_(model, class_name, fields))
}

generate_setters_getters_ <- function(model, class_name, fields) {
    methods <- list()
    for (field in names(fields)) {
        if (grepl("^.*_id$", field)[[1]]) {
            table <- sub("_id$", "", field)
            getter <- generate_fk_getter_(table, field)
            setter <- generate_fk_setter_(class_name, table)
        } else {
            getter <- generate_getter_(field)
            setter <- generate_setter_(class_name, field, fields[[field]])
        }
        methods[[sprintf("get_%s", field)]] <- getter
        methods[[sprintf("set_%s", field)]] <- setter
    }
    for (table in model$many) {
        methods[[sprintf("get_%s", field)]] <- generate_get_many(model, table)
        methods[[sprintf("add_%s", field)]] <- generate_add_many(model, class_name, table)
        methods[[sprintf("set_%s", field)]] <- generate_set_many(model, class_name, table)
    }
    return (methods)
}

generate_fk_setter_ <- function(class_name, table_name) {
    setter <- function(value) {
        table_name <- table_name
        .self$cache[[table_name]] <- value
    }
    setter <- inject_local_function_dependencies_(setter, 2, table_name)
    return (setter)
}

generate_fk_getter_ <- function(table_name, field) {
    getter <- function() {
        table_name <- table_name
        field <- field
        if (is.null(.self$cache[[table_name]])) {
            .self$cache[[table_name]] <- (
                .self$orm$model_objects_[[
                    table_name
                ]]()$load(.self[[field]])
            )
        }
        return (.self$cache[[table_name]])
    }
    getter <- inject_local_function_dependencies_(getter, 2, table_name, field)
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
    setter <- inject_local_function_dependencies_(setter, 2, field, err_string, test)
    return (setter)
}

generate_get_many <- function(schema, other_table) {
    getter <- function() {
        linkage_table_name <- linkage_table_name
        other_table <- other_table
        if (is.null(.self$cache[[other_table]])) {
            other <- .self$orm$model_objects_[[other_table]]()
            .self$cache[[other_table]] <- (
                other$load_by(
                    other$join_clause(
                        table=linkage_table_name,
                        on=OperatorClause(
                            left=other$table_field("id")$as.request(),
                            right=.self$orm$table_field(
                                table=linkage_table_name,
                                field=sprintf("%s_id", other$table__)
                            )$as.request(),
                            operator=.self$OPERATORS$EQ
                        )
                    ),
                    other$join_clause(
                        table=linkage_table_name,
                        on=OperatorClause(
                            left=.self$table_field("id")$as.request(),
                            right=.self$orm$table_field(
                                table=linkage_table_name,
                                field=sprintf("%s_id", .self$table__)
                            )$as.request(),
                            operator=.self$OPERATORS$EQ
                        )
                    )
                )
            )
        }
        return (.self$cache[[other_table]])
    }
    if (schema$table < other_table) {
        linkage_table_name <- c(schema$table, other_table)
    } else {
        linkage_table_name <- c(schema$table, other_table)
    }
    getter <- inject_local_function_dependencies_(
        getter, 2,
        linkage_table_name, other_table
    )
    return (getter)
}

generate_add_many <- function(schema, class_name, table) {
    return ()
}

generate_set_many <- function(schema, class_name, table) {
    return ()
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
#' @author Lain Pavot
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
                    if ((type == "TEXT") || (type == "BLOB")) {
                        .self[[field]] <- ""
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
        ...
    )
    do.call(generator$methods, methods)
    return (generator)
}
