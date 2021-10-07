

QueryCondition <- setRefClass(
  "QueryCondition",
  fields=list(
    .orm__="ORM",
    tree="call"
  ), methods=list(initialize=function(.orm__, condition) {
    .self$.orm__ <- .orm__
    .self$tree <- substitute(confition)
  })
)

## (function(x){print(substitute(x)[[3]][[2]])})  (id == 4 || (id == 5 && truc ~ "bidule"))


QueryJoin <- setRefClass(
  "QueryJoin",
  fields=list(
    .orm__="ORM",
    left="MetaModel",
    on="ModelField",
    right="MetaModel",
    to="ModelField",
    kind="numeric"
  )
)

as.character.QueryJoin <- function(x) {
  .self <- get_self(x)
  return (
    fill_template(
      .self$.orm__$requests_templates$join,
      left_table=.self$left$table_name,
      left_on=.self$on$name,
      right_table=.self$right$table_name,
      right_on=.self$to$name
    )
  )
}

setMethod("as.character", "QueryJoin", as.character.QueryJoin)

Query <- setRefClass(
  "Query",
  fields=list(
    .orm__="ORM",
    .selected__="ModelField",
    .from__="MetaModel",
    .joined__="MetaModel",
    .where__="QueryCondition",
    .ordered_by__="ModelField",
    .limited_by__="numeric"
  )
)

Query$methods(initialize=function(.orm__) {
  .self$.orm__ <- .orm__
  .self$.selected__ <- c()
  .self$.from__ <- c()
  .self$.joined__ <- c()
  .self$.where__ <- c()
  .self$.ordered_by__ <- c()
  .self$.limited_by__ <- -1
})

Query$methods(set_selected=function(field) {
  if (!is(field, "ModelField")) {
    stopf(
      "Query$set_selected expects a ModelField as first argument, got %s instead",
      class(field)
    )
  }
  .self$.selected__ <- c(field)
  return (.self)
})

Query$methods(add_selected=function(field) {
  if (!is(field, "ModelField")) {
    stopf(
      "Query$add_selected expects a ModelField as first argument, got %s instead",
      class(field)
    )
  }
  .self$.selected__ <- c(.self$.selected__, field)
  return (.self)
})

Query$methods(set_from=function(model) {
  if (!is(model, "MetaModel")) {
    stopf(
      "Query$set_from expects a MetaModel as first argument, got %s instead",
      class(model)
    )
  }
  .self$.from__ <- c(model)
  return (.self)
})

Query$methods(add_from=function(model) {
  if (!is(model, "MetaModel")) {
    stopf(
      "Query$add_from expects a MetaModel as first argument, got %s instead",
      class(model)
    )
  }
  .self$.from__ <- c(.self$.from__, model)
  return (.self)
})

Query$methods(join=function(
  model,
  on,
  other=NULL,
  to=NULL,
  kind=LEFT_OUTER_JOIN
) {
  if (!is(model, "MetaModel")) {
    stopf(
      "Query$join expects a MetaModel as first argument, got %s instead",
      class(model)
    )
  }
  if (!is(on, "ModelField")) {
    stopf(
      "Query$join expected a table field as second argument. Got %s instead.",
      class(on)
    )
  }
  if (!is.null(other)) {
    if (!is(other, "MetaModel")) {
      stopf(
        "Query$join expected 'other' to be a MetaModel, got a %s instead.",
        class(other)
      )
    }
    if (is.null(to)) {
      stopaste0(
        "Query$join expects the 'to' parameter to be set when ",
        "'other' is given."
      )
    }
    if (!is(to, "ModelField")) {
      stopf(
        "Query$join expected 'to' to be a ModelField, got a %s instead.",
        class(to)
      )
    }
  }
  if (length(.self$.from__) == 0) {
    .self$set_from(model)
    if (!is.null(other)) {
      .self$.joined__ <- c(QueryJoin(
        .orm__=.self$.orm__,
        left=model,
        on=on,
        right=other,
        to=to,
        kind=kind
      ))
    }
    return (.self)
  }
  if (is.null(other)) {
    if (length(.self$.joined__) == 0) {
      stopaste0(
        "Join cannot be created if no 'other' table is given and ",
        "the join chain is empty."
      )
    }
    other <- model
    to <- on
    last <- .self$.joined__[[length(.self$.joined__)]]
    model <- last$right
    on <- last$to
  } else {
    tables <- table_fits_in_join_chain(model, .self$.joined__)
    if (length(tables)) {
      stopf(
        paste0(
          "This join breaks the join chain: %s -> %s.\nThe curent",
          "tables are: %s. None of the new tables are in the chain."
        ),
        model$table_name,
        other$table_name,
        paste(tables, sep=", ")
      )
    }
  }
  .self$.joined__ <- c(
    .self$.joined__,
    QueryJoin(
      .orm__=.self$.orm__,
      left=model,
      on=on,
      right=other,
      to=to,
      kind=kind
    )
  )
  return (.self)
})

Query$methods(where=function(condition) {
  if (!is(condition, "QueryCondition")) {
    condition <- QueryCondition(.self$.orm__, condition)
  }
  .self$.where__ <- c(.self$.where__, condition)
  return (.self)
})

Query$methods(order_by=function(field) {
  if (!is(field, "ModelField")) {
    stopf(
      "Query$order_by expects a ModelField as first argument, got %s instead",
      class(field)
    )
  }
  .self$.ordered_by__ <- field
  return (.self)
})

Query$methods(limit=function(number) {
  if (!is(field, "ModelField")) {
    stopf(
      "Query$limit expects a numeric as first argument, got %s instead",
      class(number)
    )
  }
  .self$.limited_by__ <- number
  return (.self)
})
