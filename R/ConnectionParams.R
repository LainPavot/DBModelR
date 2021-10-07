

#' ConnectionParams
#' 
#' @export ConnectionParams
#' 
ConnectionParams <- setRefClass(
  "ConnectionParams",
  fields=c(
    host="character",
    port="numeric",
    user="character",
    password="character",
    database="character",
    flags="numeric",
    defaults="list",
    DBMS="character"
  )
)

as.list.ConnectionParams <- function(x) {
  .self <- get_self(x)
  return (list(
    client.flag=.self$flags,
    dbname=.self$database,
    username=.self$user,
    password=.self$password,
    host=.self$host,
    port=.self$port
  ))
}

ConnectionParams$methods(initialize=function(
  host=NULL,
  port=NULL,
  user=NULL,
  password=NULL,
  database=NULL,
  flags=NULL,
  DBMS=NO_DBMS
) {
  if (!(DBMS %in% DBMS_LIST)) {
    stopf(
      paste0(
        "Bad DBMS: %s. Expected any of <%s>",
        "You should use constant names to prevent errors: %s.",
        sep="\n"
      ),
      paste(DBMS_LIST, collapse=","),
      paste_and_last(DBMS_CONST_NAMES, collapse=", ", last=" or ")
    )
  }
  .self$set_dbms(DBMS)
  if (!is.null(host)) {
    .self$host <- host
  }
  if (!is.null(port)) {
    .self$port <- port
  }
  if (!is.null(user)) {
    .self$user <- user
  }
  if (!is.null(password)) {
    .self$password <- password
  }
  if (!is.null(database)) {
    .self$database <- database
  }
  if (!is.null(flags)) {
    .self$flags <- flags
  }
})

ConnectionParams$methods(set_dbms=function(dbms) {
  .self$DBMS <- dbms
  .self$defaults <- DEFAULT_CO_PARAMS[[DBMS]]
  .self$port <- .self$defaults$port
  .self$host <- .self$defaults$host
  .self$user <- .self$defaults$user
  .self$password <- .self$defaults$password
  .self$database <- .self$defaults$database
  .self$flags <- .self$defaults$flags
})