#' 
#' @import DBI
#' @importFrom methods selectMethod setRefClass is
#' 
NULL

#' IntegerField
#' 
#' @export IntegerField
#' 
IntegerField <- setRefClass(
  "IntegerField",
  fields=c(
    default="numeric",
    value="numeric"
  ),
  contains="ModelField"
)

#' BooleanField
#' 
#' @export BooleanField
#' 
BooleanField <- setRefClass(
  "BooleanField",
  fields=c(
    default="logical",
    value="logical"
  ),
  contains="ModelField"
)

#' CharacterField
#' 
#' @export CharacterField
#' 
CharacterField <- setRefClass(
  "CharacterField",
  fields=c(
    default="character",
    value="character"
  ),
  contains="ModelField"
)

#' FloatField
#' 
#' @export FloatField
#' 
FloatField <- setRefClass(
  "FloatField",
  fields=c(
    default="numeric",
    value="numeric"
  ),
  contains="ModelField"
)

if (requireNamespace("blob", quietly=TRUE)) {
  BlobField <- setRefClass(
    "BlobField",
    fields=c(
      value="blob",
      default="blob"
    ),
    contains="ModelField"
  )
} else {
  BlobField <- setRefClass(
    "BlobField",
    contains="ModelField",
    methods=list(initialize=function(...) {
      stop("Need blob package to use this field kind.")
    })
  )
}

#' BlobField
#' 
#' @export BlobField
#' 
BlobField <- BlobField

#' DateField
#' 
#' @export DateField
#' 
DateField <- setRefClass(
  "DateField",
  fields=c(
    default="POSIXct",
    value="POSIXct"
  ),
  contains="ModelField"
)

#' ForeignKeyField
#' 
#' @export ForeignKeyField
#' 
ForeignKeyField <- setRefClass(
  "ForeignKeyField",
  fields=c(
    lazy="logical",
    .needs_loading_obj__="logical",
    foreign_table="MetaModel",
    foreign_table_gen="refObjectGenerator",
    foreign_table_class="character",
    fk_field="list",
    kind="numeric",
    value="ANY",
    id="numeric"
  ),
  contains="ModelField",
  methods=list(initialize=function(
    model,
    fk_field,
    kind=ONE_TO_MANY,
    value=NULL,
    lazy=TRUE,
    id=numeric(0),
    ...
  ) {
    callSuper(...)
    if (!(kind %in% c(MANY_TO_MANY, ONE_TO_MANY, MANY_TO_ONE))) {
      stopf(
        paste0(
          "Bad foreign key kind. Expected any of %s (%s), ",
          "%s (%s) or %s (%s) , got %s"
        ),
        MANY_TO_MANY, "MANY_TO_MANY",
        ONE_TO_MANY, "ONE_TO_MANY",
        MANY_TO_ONE, "MANY_TO_ONE",
        kind
      )
    }
    if (!is(model, "MetaModel")) {
      stopf(
        "Expected a MetaModel as first argument, got a %s",
        class(model)
      )
    }
    if (!is.list(fk_field)) {
      stopf(
        "Expected a list as second argument, got a %s",
        class(fk_field)
      )
    }
    if (!is.character(fk_field$this) || !is.character(fk_field$other)) {
      stop(
        "fk fields expected to have a 'this' and a 'other' names."
      )
    }
    .self$foreign_table <- model
    .self$kind <- kind
    .self$value <- value
    .self$foreign_table_gen <- model$getRefClass()
    .self$foreign_table_class <- class(model)
    .self$fk_field <- fk_field
    .self$.needs_loading_obj__ <- TRUE
    .self$lazy <- lazy
    .self$id <- id
    if (!identical(.self$id, numeric(0)) && !.self$lazy) {
      .self$load_obj(model)
    }
  })
)
