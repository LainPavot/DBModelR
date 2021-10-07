#' 
#' @import DBI
NULL

#' ORM
#' 
#' @export ORM
#' 
ORM <- setRefClass(
  "ORM",
  fields=c(
    connection_parameters="ConnectionParams",
    connector="DBIConnection",
    dbms_env="environment",
    connected=function() {
      return (
        .self$has_dbms
        && !is.null(.self$connector)
        && .self$dbms_env[["dbIsValid"]](.self$connector)
      )
    },
    has_dbms=function() {
      return (!identical(names(.self$dbms_env), character(0)))
    },
    host=function() {
      return (.self$connection_parameters$host)
    },
    port=function() {
      return (.self$connection_parameters$port)
    },
    user=function() {
      return (.self$connection_parameters$user)
    },
    database=function() {
      return (.self$connection_parameters$database)
    },
    flags=function() {
      return (.self$connection_parameters$flags)
    },
    DBMS=function() {
      return (.self$connection_parameters$DBMS)
    }
  )
)

print.ORM <- function(x) {
  .self <- get_self(x)
}

ORM$methods(reload_dbms=function() {
  if (.self$DBMS != NO_DBMS) {
    old <- .self$connection_parameters$DBMS
    .self$set_dbms(NO_DBMS)
    .self$set_dbms(old)
  }
})

ORM$methods(set_params=function(co_param) {
  .self$disconnect()
  .self$connection_parameters <- co_param
  .self$reload_dbms()
})

ORM$methods(set_param=function(name, value, reconnect=FALSE) {
  .self$disconnect()
  .self$connection_parameters$field(name, value)
  if (reconnect) {
    .self$connect()
  }
})

ORM$methods(set_dbms=function(dbms) {
  .self$disconnect()
  if (dbms == NO_DBMS) {
    .self$dbms_env <- new.env()
    .self$connection_parameters$DBMS <- NO_DBMS
    return ()
  }
  if (!is.null(package <- DBMS_PACKAGES[[dbms]])) {
    if(!require(package, character.only=TRUE, quietly=TRUE)) {
      stopf(
        "You must install %s before you set the dbms to %s.",
        package, dbms
      )
    }
    .self$dbms_env <- as.environment(sprintf("package:%s", package))
    .self$connection_parameters$DBMS <- dbms
  } else {
    stopf(
      "Unknown DBMS: %s. Possible DBMS: %s",
      dbms,
      names(DBMS_PACKAGES)
    )
  }
})

ORM$methods(disconnect=function() {
  if (.self$connected) {
    .self$dbms_env[["dbDisconnect"]](.self$connector)
  }
})

ORM$methods(connect=function() {
  if (.self$connected) {
    warning("Already connected.")
    return ()
  }
  if (!.self$has_dbms || .self$DBMS == NO_DBMS) {
    stop("No DBMS loaded. Please, use ORM$set_dbms.")
  }
  if (!.self$connected) {
    driver_name <- DBMS_DRIVER[[.self$DBMS]]
    driver <- .self$dbms_env[[driver_name]]()
    .self$connector <- do.call(
      .self$dbms_env[["dbConnect"]],
      c(
        driver,
        as.list(.self$connection_parameters)
      )
    )
  } else {
    warning("Already connected.")
  }
})

ORM$methods(model_builder=function() {
  return (ModelBuilder(.self))
})

ORM$methods(model_field_to_SQL_type=function(R_type) {
  if (length(R_type) > 1) {
    return (sapply(R_type, .self$model_field_to_SQL_type))
  }
  if (is(R_type, "FloatField")) {
    ## Float is somewhat special because the double field
    ## Does not seems to work with S5 classes.
    ## We use numeric type to store a double type.
    R_type <- "double"
  } else if (is(R_type, "ForeignKeyField")) {
    R_type <- "object"
  } else {
    R_type <- R_type$type
  }
  return (.self$R_type_to_SQL_type(R_type))
})

ORM$methods(R_type_to_SQL_type=function(R_type) {
  if (length(R_type) > 1) {
    return (sapply(R_type, .self$R_type_to_SQL_type))
  }
  if (is(R_type, "ModelField")) {
    return (.self$model_field_to_SQL_type(R_type))
  }
  if (is.null(mapper <- TYPES_MAPPINGS[[.self$DBMS]])) {
    stopf("ORM<R_type_to_SQL_type> Unknown DBMS: %s", .self$DBMS)
  }
  if (is.null(SQL_type <- mapper[[R_type]])) {
    stopf("ORM<R_type_to_SQL_type> R type has no mapping: %s.", R_type)
  }
  return (SQL_type)
})

ORM$methods(load_object_by_id=function(model, id) {
  key <- c(model$table_name, id)
  if (!exists(key, envir=.self$obj_cache)) {
    return (.self$populate_cache(model, id))
  }
  return (get(key, envir=.self$obj_cache))
})

ORM$methods(populate_cache=function(model, id) {
  object <- .self$query(model)
  assign(c(model$table_name, id), object, envir=.self$obj_cache)
  return (object)
})

ORM$methods(query=function(...) {
  query_obj <- Query(.orm__=.self)
  for (obj in c(...)) {
    query_obj$join(obj)
  }
  return (query_obj)
})

