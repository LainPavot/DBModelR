

#' ModelField
#' 
#' @export ModelField
#' 
ModelField <- setRefClass(
  "ModelField",
  fields=c(
    name="character",
    type="character",
    is.na="logical",
    is.null="logical",
    NAable="logical",
    nullable="logical",
    .fieldClasses__="list",
    .has_default__="logical",
    .has_been_set__="logical",
    .needs_loading__="logical",
    .has_been_loaded__="logical",
    .has_been_updated__="logical",
    .has_been_assigned__="logical"
  )
)

ModelField$methods(initialize=function(
  default=NULL,
  is.na=FALSE,
  is.null=FALSE,
  NAable=FALSE,
  nullable=FALSE,
  ...
) {
  callSuper(...)
  .self$.has_default__ <- !is.null(default)
  .self$is.na <- is.na
  .self$is.null <- is.null
  .self$NAable <- NAable
  .self$nullable <- nullable
  .self$.needs_loading__ <- FALSE
  .self$.has_been_loaded__ <- FALSE
  .self$.has_been_assigned__ <- FALSE
  .self$.has_been_set__ <- FALSE
  .self$.has_been_updated__ <- FALSE
  if (.self$.has_default__) {
    .self$default <- default
  }
  .self$.fieldClasses__ <- .self$getRefClass()@generator$def@fieldClasses
  if (!is.null(.type <- .self$.fieldClasses__$value)) {
    .self$type <- .type
  }
  # .self$.has_default__ <- ("default" %in% names(.self$.fieldClasses__))
})

ModelField$methods(get=function(model) {
  if (.self$.has_been_set__) {
    return (.self$value)
  }
  if (.self$.needs_loading__) {
    return (.self$load(model))
  }
  if (.self$.has_default__) {
    return (.self$default)
  }
  return (NULL)
})

ModelField$methods(set=function(value, loaded=FALSE) {
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

ModelField$methods(load=function(model) {
  .self$.needs_loading__ <- FALSE
  return (NULL)
})