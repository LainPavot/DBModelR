

TableMeta$methods(initialize=function(table_name="Unknown", ...) {
  callSuper()
  name <- table_name
  if (is.character(name)) {
    name <- TableName(name)
  }
  args <- list(...)
  if (length(args) != length(names(args))) {
    stop(paste0(
      "Models fields must hold a name.\n",
      "Get some help with: ?ORM$ModelBuilder"
    ))
  }
  filter <- sapply(args, function(x)!is(x, "ModelField"))
  if (any(filter)) {
    stopf(
      "Expected ModelField instances, got %s.",
      lapply(args[filter], class)
    )
  }
  .self$table_name <- name
  .self$fields <- args

  for (name in names(.self$fields)) {
    .self$fields[[name]]$name <- name
  }
})

`%in%.TableMeta` <- function(x, table) {
  .self <- get_self(table)
  return (x %in% .self$fields)
}
setMethod("%in%", c("list", "ModelField"), `%in%.TableMeta`)
