

#' `$`
#' 
#' @description Returns asked method or attribute's value.
#' If the "lazy" parameter of the meta attribute is TRUE and the attribute
#' is a ModelField, the value is queried from the database and stored in 
#' the object.
#' If the value is already stored in the ModelField, the attribute is
#' directly returned.
#' 
#' @param x A MetaModel subclass' instance.
#' @param name The name of the attribute/method
#' 
#' @return The model's attributes or method.
#' 
dash.MetaModel <- function(x, name) {
  self.get <- selectMethod("$", "envRefClass")
  meta <- self.get(x, ".meta__")
  if (!is.null(field <- meta$fields[[name]])) {
    return (field$get(get_self()))
  }

  if (any(grepl("^\\.raw_\\S+__$", name))) {
    name <- substr(name, 6, nchar(name)-2)
    if (!is.null(field <- meta$fields[[name]])) {
      return (field)
    }
  }
  if (exists(name, envir=x)) {
    return (self.get(x, substitute(name)))
  }
  stopf(
    "Bad field name for model %s: %s is not defined.",
    class(get_self(x))[[1]],
    name
  )
}

#' `$<-`
#' 
#' @description Assign the value to the field of the model instance.
#' If the "lazy" parameter of the meta attribute is TRUE and the attribute
#' is a ModelField, the value is queried from the database and stored in 
#' the object.
#' If the value is already stored in the ModelField, the attribute is
#' directly returned.
#' 
#' @param x A MetaModel subclass' instance.
#' @param name The name of the attribute/method
#' @param value The name of the attribute/method
#' 
#' @return The model's attributes or method.
#' 
dash.assign.MetaModel <- function(x, name, value) {
  self.get <- selectMethod("$", "envRefClass")
  meta <- self.get(x, ".meta__")
  if (!is.null(field <- meta$fields[[name]])) {
    field$set(value)
  } else {
    selectMethod("$<-", "envRefClass")(x, substitute(name), value)
  }
  return (get_self(x))
}

as.list.MetaModel <- function(x, ...) {
  .self <- get_self(x)

  return (value)
}

as.vector.MetaModel <- function(x) {
  return (as.vector(as.list(set_self(x))))
}

as.character.MetaModel <- function(x) {
  .self <- get_self(x)
  if (length(fields <- .self$fields) == 0) {
    return (paste0(
      as.character(.self$table_name),
      "[no_any_field]"
    ))
  }
  sep <- "\n  "
  return (sprintf(
    "%sModel[%s%s\n]",
    .self$table_name,
    sep,
    paste(sprintf(
      "%s<%s:%s>",
      names(fields),
      getattr(fields, "type"),
      .self$.orm__$model_field_to_SQL_type(fields)
    ), collapse=paste0(",", sep)
    )
  ))
}

setMethod("$", "MetaModel", dash.MetaModel)
setMethod("$<-", "MetaModel", dash.assign.MetaModel)
setMethod("as.list", "MetaModel", as.list.MetaModel)
setMethod("as.list.default", "MetaModel", as.list.MetaModel)
setMethod("as.vector", "MetaModel", as.vector.MetaModel)
setMethod("as.character", "MetaModel", as.character.MetaModel)
setMethod("as.character.default", "MetaModel", as.character.MetaModel)

MetaModel$methods(initialize=function(.orm__=ORM(), .meta__=TableMeta()) {
  help <- paste0(
    "You should use the ",
    "`model <- ORM$ModelBuilder(model_name, fields)` ",
    "method to create your models as it helps providing this ",
    "parameters automatically.\n",
    "Get some help with: ?MetaModel"
  )
  template <- paste0("%s Got %s.\n", help)
  if (!is(.orm__, "ORM")) {
    stopf(
      template,
      "Expected ORM instance as first argument.",
      class(.orm__)
    )
  }
  if (!is(.meta__, "TableMeta")) {
    stopf(
      template,
      "Expected TableMeta instance as second argument.",
      class(.meta__)
    )
  }
  .self$.orm__ <- .orm__
  .self$.meta__ <- .meta__
})

MetaModel$methods(query=function() {
  return (.self$.orm__$query(.self))
})