

#' ForeignKeyField$set
#'
#' @param value The value to assign to this field
#' @param name The name of this field
#' @param loaded Tells the objects if the field has been loaded from
#' the database or not. Default is FALSE
#' @return .self
#'
ForeignKeyField$methods(set=function(value, loaded=FALSE) {
  if (!is(value, .self$foreign_table_class)) {
    stopf(
      "ForeignKeyField$%s expected a %s object, got a %s instead.",
      .self$name,
      .self$foreign_table_class,
      class(value)
    )
  }
  .self$value <- value

  .self$.has_been_set__ <- TRUE
  if (loaded) {
    .self$.has_been_loaded__ <- TRUE
    .self$.has_been_assigned__ <- FALSE
    .self$.has_been_updated__ <- FALSE
  } else {
    .self$.has_been_assigned__ <- TRUE
    if (.self$.has_been_loaded__) {
      .self$.has_been_updated__ <- TRUE
    }
  }
  .self$value <- value
  return (.self)
})

#' ForeignKeyField$load
#'
#' Loads other models' fk from the database (or cache).
#' Loads related models if .self$lazy is false.
#'
#' @param model The model this field is associated with.
#'  objects retrieved from the database.
#' @return .self
#'
ForeignKeyField$methods(load=function(model) {
  callSuper(model)
  if (!.self$lazy) {
    .self$load_obj(model)
  }
  return (NULL)
})

#' ForeignKeyField$load_obj
#'
#' Loads other models' objects from the database (or cache).
#'
#' @param model The model this field is associated with.
#'  objects retrieved from the database.
#' @return .self
#'
ForeignKeyField$methods(load_obj=function(model) {
  if (!.needs_loading_obj__) {
    return (.self$value)
  }
  .self$.needs_loading_obj__ <- FALSE
  obj <- model$.orm__$load_object_by_id(
    model=.self$get_template_table(),
    id=.self$id
  )
  .self$value <- obj
  return (.self$value)
})

#' ForeignKeyField$save
#'
#' Saves foreign object if needed, save this model and then
#' creates linkages entries if needed. Add new objects to the
#' global_cache
#'
#' @param model The model this field is associated with.
#'  objects retrieved from the database.
#' @return .self
#'
ForeignKeyField$methods(save=function(model) {
  .self$.needs_loading__ <- FALSE
  .self$.has_been_loaded__ <- TRUE
  .self$.has_been_updated__ <- FALSE
  .self$.has_been_set__ <- TRUE
  return (NULL)
})

#' ForeignKeyField$get_template_table
#'
#' @return A new instance of the model associated with this model.
#'
ForeignKeyField$methods(get_template_table=function() {
  return (.self$foreign_table_gen(
    .self$foreign_table$.orm__,
    .self$foreign_table$.meta__
  ))
})