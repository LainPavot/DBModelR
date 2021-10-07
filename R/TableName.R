
#' TableName
#' 
#' @export TableName
#' 
TableName <- setRefClass(
  "TableName",
  fields=c(
    name="character"
  )
)

as.character.TableName <- function(x) {
  return (get_self(x)$name)
}

setMethod("as.character", "TableName", as.character.TableName)
setMethod("as.character.default", "TableName", as.character.TableName)


TableName$methods(initialize=function(name=character(0)) {
  .self$name <- name
})