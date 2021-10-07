

#' TableMeta
#' 
#' @export TableMeta
#' 
TableMeta <- setRefClass(
  "TableMeta",
  fields=c(
    table_name="TableName",
    fields="list"
  )
)

#' MetaModel
#' 
#' @description
#' This class defines a super class for every model used by the ORM.
#' The prefered method to create a model is to use the ORM's
#' method `ModelBuilder`, like this:
#' 
#' Person <- ORM$ModelBuilder(
#'   "Person",
#'   name=CharacterField(),
#'   nickname=CharacterField(nullable=TRUE),
#'   adress=ForeignKeyField(Adress, type=MANY_TO_MANY, nullable=TRUE)
#' )
#' 
#' 
#' You can also inheritate from this class for more flexible behaviours
#' and advenced uses. RefClass is the prefered class style for models.
#' 
#' If you decide to implement models yourself, please, remember
#' the first parameter of models must be the ORM itself, and the second
#' parameters must be a TableMeta object, containing at least the table
#' name and the fields corresponding the in-base.
#' 
#' The code from the previous section is more or less equivalant to:
#' 
#' Person <- (function(orm) {
#'   meta <- TableMeta(
#'     table_name=TableName("Person"),
#'     name=CharacterField(),
#'     nickname=CharacterField(nullable=TRUE),
#'     adress=ForeignKeyField(Adress, type=MANY_TO_MANY, nullable=TRUE)
#'   )
#'   model <- setRefClass(
#'     "Person",
#'     contains=c("MetaModel")
#'   )
#'   return (function(...) {
#'     model(orm__=orm, meta__=meta, ...)
#'   })
#' })(orm)
#' 
#' 
#' @export MetaModel
#' 
MetaModel <- setRefClass(
  "MetaModel",
  fields=c(
    .orm__="ORM",
    .meta__="TableMeta",
    table_name=function() {
      return (.self$.meta__$table_name$name)
    },
    fields=function() {
      return (.self$.meta__$fields)
    },
    field_names=function() {
      return (names(.self$.meta__$fields))
    }
  )
)
