
FORBIDEN_FIELDS <- tolower(c(
    ## sqlite keywords
    c(
        "ABORT","ACTION","ADD","AFTER","ALL","ALTER","ALWAYS","ANALYZE","AND",
        "AS","ASC","ATTACH","AUTOINCREMENT","BEFORE","BEGIN","BETWEEN","BY",
        "CASCADE","CASE","CAST","CHECK","COLLATE","COLUMN","COMMIT","CONFLICT",
        "CONSTRAINT","CREATE","CROSS","CURRENT","CURRENT_DATE","CURRENT_TIME",
        "CURRENT_TIMESTAMP","DATABASE","DEFAULT","DEFERRABLE","DEFERRED",
        "DELETE","DESC","DETACH","DISTINCT","DO","DROP","EACH","ELSE","END",
        "ESCAPE","EXCEPT","EXCLUDE","EXCLUSIVE","EXISTS","EXPLAIN","FAIL",
        "FILTER","FIRST","FOLLOWING","FOR","FOREIGN","FROM","FULL","GENERATED",
        "GLOB","GROUP","GROUPS","HAVING","IF","IGNORE","IMMEDIATE","IN","INDEX",
        "INDEXED","INITIALLY","INNER","INSERT","INSTEAD","INTERSECT","INTO",
        "IS","ISNULL","JOIN","KEY","LAST","LEFT","LIKE","LIMIT","MATCH",
        "NATURAL","NO","NOT","NOTHING","NOTNULL","NULL","NULLS","OF","OFFSET",
        "ON","OR","ORDER","OTHERS","OUTER","OVER","PARTITION","PLAN","PRAGMA",
        "PRECEDING","PRIMARY","QUERY","RAISE","RANGE","RECURSIVE","REFERENCES",
        "REGEXP","REINDEX","RELEASE","RENAME","REPLACE","RESTRICT","RIGHT",
        "ROLLBACK","ROW","ROWS","SAVEPOINT","SELECT","SET","TABLE","TEMP",
        "TEMPORARY","THEN","TIES","TO","TRANSACTION","TRIGGER","UNBOUNDED",
        "UNION","UNIQUE","UPDATE","USING","VACUUM","VALUES","VIEW","VIRTUAL",
        "WHEN","WHERE","WINDOW","WITH","WITHOUT"
    ),

    ## models' functions defined by the orm
    c(
        "initialize", "load", "load_by", "save"
    ),

    ## models' attributes defined by the orm
    c(
        "modified__", "modified__", "sql_model__", "table__",
        "orm__", "model_name__", "fields__"
    )
))


#' @export
ModelDefinition$methods(
    initialize=function(table="unknown", fields=list(), fk=list()) {
    ## The regex in this methods never use the [A-Z] range because
    ## grepl must use the ignore.case systematically
    ## But, error messages must show the [A-Z] range in the regex
    ## to inform users they can use uppercasse.
    field_regex <- "^[a-z]+[a-z0-9_]+$"
    field_regex_error_message <- "^[A-Za-z]+[A-Za-z0-9_]+$"
    if (any(grepl(sprintf("^%s$", table), FORBIDEN_FIELDS, ignore.case=TRUE))) {
        stop(paste("The table name", table, "is forbiden."))
    }
    if (!grepl(field_regex, table, perl=TRUE, , ignore.case=TRUE)) {
        stop("ModelDefinition$table must match", field_regex_error_message)
    }
    if (!is.list(fields)) {
        stop("ModelDefinition$fields must be a list of strings.")
    }
    if (any(names(fields) == "")) {
        stop("ModelDefinition$fields must have a name for each element.")
    }
    for (field in names(fields)) {
        if (!is.character(fields[[field]])) {
            stop("ModelDefinition$fields must be a list of strings.")
        }
        if (!grepl(field_regex, field, perl=TRUE, ignore.case=TRUE)) {
            stop(
                "ModelDefinition$fields must contain names that match",
                field_regex_error_message
            )
        }
        if (any(grepl(sprintf("^%s$", field), FORBIDEN_FIELDS))) {
            stop(sprintf("The field name %s is forbiden (%s model).", field, table))
        }
    }
    .self$table <- table
    .self$fields <- fields
    .self$fk <- fk
})
