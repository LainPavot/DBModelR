

require_blob <- function() {
    if (!requireNamespace("blob", quietly=TRUE)) {
        stop("You need to install the package \"blob\" to uses this type")
    }
}


FIELDS_CONVERTERS <- list(
    INTEGER=function() return (as.numeric),
    TRUE_INTEGER=function() return (as.integer),
    TEXT=function() return (as.character),
    POSIXT=function() return (as.POSIXct),
    FLOAT=function() return (as.numeric),
    REAL=function() return (as.numeric),
    BOOLEAN=function() return(as.logical),
    BLOB=function() {
        require_blob()
        return (blob::as.blob)
    }
)

FIELDS_DEFAULT_VALUES <- list(
    INTEGER=function() return(0),
    TRUE_INTEGER=function() return(0),
    TEXT=function() return(""),
    FLOAT=function() return(0.0),
    REAL=function() return(0),
    POSIXT=function() return(as.POSIXct(Sys.time())),
    BOOLEAN=function() return(FALSE),
    BLOB=function() {
        require_blob()
        return (blob::as_blob(""))
    }
)

SQLITE_FIELD_TO_R_TYPE <- list(
    INTEGER=function() return("numeric"),
    TRUE_INTEGER=function() return("integer"),
    TEXT=function() return("character"),
    FLOAT=function() return("numeric"),
    REAL=function() return("numeric"),
    POSIXT=function() return("POSIXt"),
    BOOLEAN=function() return("logical"),
    BLOB=function() {
        require_blob()
        return ("blob")
    }
)

get_from_constant_ <- function(field, constant_table) {
    field <- toupper(field)
    if (is.null(getter <- constant_table[[field]])) {
        stop(sprintf(
            "Unknown type: %s. Expected any of %s",
            field, paste(names(constant_table), collapse=", ")
        ))
    }
    return (getter())
}

get_converter_for <- function(field) {
    return (get_from_constant_(field, FIELDS_CONVERTERS))
    field <- toupper(field)
    if (is.null(converter <- FIELDS_CONVERTERS[[field]])) {
        stop(sprintf(
            "Unknown type: %s. Expected any of %s",
            field, paste(names(FIELDS_CONVERTERS), collapse=", ")
        ))
    }
    return (converter())
}

get_default_value_for <- function(field) {
    return (get_from_constant_(field, FIELDS_DEFAULT_VALUES))
    field <- toupper(field)
    if (is.null(default_value <- FIELDS_DEFAULT_VALUES[[field]])) {
        stop(sprintf(
            "Unknown type: %s. Expected %s",
            field, paste(names(FIELDS_DEFAULT_VALUES), collapse=", ")
        ))
    }
    return (default_value())
}

sqlite_field_to_R_type_ <- function(field) {
    return (get_from_constant_(field, SQLITE_FIELD_TO_R_TYPE))
    field <- toupper(field)
    if (is.null(r_type <- SQLITE_FIELD_TO_R_TYPE[[field]])) {
        stop(sprintf(
            "Unknown type: %s. Expected %s",
            field, paste(names(SQLITE_FIELD_TO_R_TYPE), collapse=", ")
        ))
    }
    return (r_type())
}

sqlite_fields_to_R_types_ <- function(list) {
    for (i in seq_along(list)) {
        list[[i]] <- sqlite_field_to_R_type_(list[[i]])
    }
    return (list)
}

generate_methods_ <- function(model, class_name, fields) {
    methods <- generate_setters_getters_(model, class_name, fields)
    return (methods)
}

generate_setters_getters_ <- function(model, class_name, fields, methods=list()) {
    for (field in names(fields)) {
        if (any(grepl("^.*_id$", field))) {
            table <- sub("_id$", "", field)
            if (any(grepl(sprintf("^%s$", table), model$one))) {
                getter <- generate_fk_getter_(table, field)
                setter <- generate_fk_setter_(class_name, table)
                field <- table
                methods[[sprintf("get_%s", field)]] <- getter
                methods[[sprintf("set_%s", field)]] <- setter
                next
            }
        }
        getter <- generate_getter_(field)
        setter <- generate_setter_(class_name, field, fields[[field]])
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
        class_name <- class_name
        if (!is(value, class_name)) {
            stop(sprintf(
                "Bad value for fk field. Expected <%s>, got <%s>.",
                class_name, class(value)
            ))
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
        .self[[field]] <- value$get_id()
        return (.self)
    }
    field <- sprintf("%s_id", table_name)
    setter <- inject_local_function_dependencies_(
        setter, 2, field, class_name_from_table_name_(table_name)
    )
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
    } else if (type == "POSIXt") {
        test <- function(x) {is(x, "POSIXt")}
    } else if (type == "integer") {
        test <- is.integer
    } else if (type == "blob") {
        test <- function(x)is(x, "blob")
    } else {
        stop(sprintf("Unknown filed type: %s", type))
    }
    err_string <- sprintf(
        "Bad field type for %s$%s.
        Type <%s> expected, got <%%s> with value: %%s",
        class_name, field, type
    )
    setter <- function(new_value) {
        field <- field
        err_string <- err_string
        test <- test
        if (missing(new_value)) {
            return(.self$field(field, new_value))
        } else if (!test(new_value)) {
            stop(sprintf(err_string, class(new_value), new_value))
        }
        if (is(new_value, "blob") && length(new_value) == 0) {
            warning(paste(
                "Blobs must contains at least one raw",
                "vector, like this: `blob::blob(raw())`"
            ))
            new_value <- blob::as_blob("")
        }
        if (.self[[field]] == new_value) {
            return (.self)
        }
        if (is.null(.self$modified__[[field]])) {
            ## if not modified yet, add a "modified" flag for this field
            if (.self$loaded__) {
                .self$modified__[[field]] <- .self[[field]]
            } else {
                .self$modified__[[field]] <- new_value
            }
        } else if (.self$modified__[[field]] == new_value) {
            ## if already modified and back to ariginal new_value,
            ## remove the "modified" flag for this field
            .self$modified__[[field]] <- NULL
        }
        .self[[field]] <- new_value
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
        if (is(value, "ModelMeta")) {
            value <- list(value)
        } else if (is(value, "ResultSet")) {
            value <- as.list(value)
        }
        if (!is(value, "list")) {
            stop(sprintf(
                "Bad type: expected ModelMeta or ResultSet, got %s",
                class(value)
            ))
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
        if (is(value, "ModelMeta")) {
            value <- list(value)
        } else if (is(value, "ResultSet")) {
            value <- as.list(value)
        } else if (!is(value, "list")) {
            stop(sprintf(
                "Bad type: expected ModelMeta or ResultSet, got %s",
                class(value)
            ))
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
        if (is(value, "ModelMeta")) {
            value <- list(value)
        } else if (is(value, "ResultSet")) {
            value <- as.list(value)
        }
        if (!is(value, "list")) {
            stop(sprintf(
                "Bad type: expected ModelMeta or ResultSet, got %s",
                class(value)
            ))
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

build_fields_converters_ <- function(fields) {
    converters <- list()
    for (field_name in names(fields)) {
        converters[[field_name]] <- get_converter_for(fields[[field_name]])
    }
    return (converters)
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
    field_converters <- build_fields_converters_(model$fields)
    methods <- generate_methods_(model, class_name, fields)
    methods[["initialize"]] <- function(...) {
        callSuper(...)
        .self$sql_model__ <- model
        .self$model_name__ <- class_name
        .self$orm__ <- orm
        .self$field_converters__ <- field_converters
        .self$table__ <- .self$sql_model__$table
        .self$defaults <- .self$sql_model__$defaults
        .self$fields__ <- .self$sql_model__$fields
        params <- list(...)
        .self$id <- .self$NOT_CREATED
        for(field in names(params)) {
            .self$modified__[[field]] <- .self$get_default_value_for(field)
            tryCatch({
                .self[[field]] <- .self$field_converters__[[field]](
                    params[[field]]
                )
            }, error=function(e) {
                .self$modified__[[field]] <- NULL
            })
        }
    }
    methods$initialize <- inject_local_function_dependencies_(
        methods$initialize, 3,
        model,
        class_name,
        orm,
        field_converters
    )
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
