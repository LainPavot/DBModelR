
message_obj <- function(x) {
  message(paste(capture.output(print(x)), collapse="\n"))
}

get_self <- function(x) {
  return (selectMethod("$", "envRefClass")(x, ".self"))
}

stopf <- function(...) {
  stop(sprintf(...))
}

stopaste <- function(...) {
  stop(paste(...))
}

stopaste0 <- function(...) {
  stop(paste0(...))
}

messagef <- function(...) {
  message(sprintf(...))
}

messpaste <- function(...) {
  message(paste(...))
}

paste_and_last <- function(args, last=" or ", ...) {
  if (length(args) == 1) {
    return (args[[1]])
  }
  if (length(args) == 2) {
    return (paste(args, collapse=last))
  }
  return (paste(
    paste(args[seq_len(length(args)-1)], ...),
    args[[length(args)]],
    sep=last
  ))
}

#' ModelBuilder
#' 
#' @param .orm__ the curent orm to attach the model to.
#' 
#' @description
#' This method is a helper to create models.
#' The first parameter is the name of the table.
#' Other parameters must be named and hold a ModelField instance, like
#' this:
#'
#' Person <- ORM$ModelBuilder(
#'   "Person",
#'   name=CharacterField(),
#'   nickname=CharacterField(nullable=TRUE),
#'   adress=ForeignKeyField(Adress, type=MANY_TO_MANY, nullable=TRUE)
#' )
#'
#' The model can then be used to manipulate the database's content.
#' See the MetaModel help for more informations:
#' ?MetaModel
#'
ModelBuilder <- function(.orm__) {
  return (function(table_name, ...) {
    if (!is.character(table_name)) {
      stopf(
        paste0(
          "Expected character as first argument of",
          "ORM$ModelBuilder. Got %s."
        ),
        class(table_name)
      )
    }
    .meta__ <- TableMeta(table_name=table_name, ...)
    model <- setRefClass(
      table_name,
      contains="MetaModel",
      where=.GlobalEnv
    )
    orm <- .orm__
    meta <- .meta__
    return (function(.orm__=orm, .meta__=meta, .model__=model) {
      .model__(.orm__=.orm__, .meta__=.meta__)
    })
  })
}

attrgetter <- function(attr_name) {
  getter <- function(x) {
    if (length(x) > 1) {
      return (sapply(x, getter))
    }
    return (x$field(attr_name))
  }
  return (getter)
}

getattr <- function(x, attr_name) {
  return (attrgetter(attr_name)(x))
}


table_fits_in_join_chain <- function(model, join_chain) {
  tables <- c()
  for (join in join_chain) {
    tables <- c(tables, join$left, join$right)
    if (
      join$left$table_name == model$table_name
      || join$right$table_name == model$table_name
    ) {
      return (c())
      break
    }
  }
  return (unique(tables))
}