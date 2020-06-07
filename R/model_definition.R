
FORBIDEN_FIELDS <- tolower(c(
    ## sqlite keywords
    c(
        "ABORT","ACTION","ADD","AFTER","ALL","ALTER","ALWAYS","ANALYZE",
        "AND","AS","ASC","ATTACH","AUTOINCREMENT","BEFORE","BEGIN",
        "BETWEEN","BY","CASCADE","CASE","CAST","CHECK","COLLATE",
        "COLUMN","COMMIT","CONFLICT","CONSTRAINT","CREATE","CROSS",
        "CURRENT","CURRENT_DATE","CURRENT_TIME","CURRENT_TIMESTAMP",
        "DATABASE","DEFAULT","DEFERRABLE","DEFERRED","DELETE","DESC",
        "DETACH","DISTINCT","DO","DROP","EACH","ELSE","END","ESCAPE",
        "EXCEPT","EXCLUDE","EXCLUSIVE","EXISTS","EXPLAIN","FAIL",
        "FILTER","FIRST","FOLLOWING","FOR","FOREIGN","FROM","FULL",
        "GENERATED","GLOB","GROUP","GROUPS","HAVING","IF","IGNORE",
        "IMMEDIATE","IN","INDEX","INDEXED","INITIALLY","INNER","INSERT",
        "INSTEAD","INTERSECT","INTO","IS","ISNULL","JOIN","KEY","LAST",
        "LEFT","LIKE","LIMIT","MATCH","NATURAL","NO","NOT","NOTHING",
        "NOTNULL","NULL","NULLS","OF","OFFSET","ON","OR","ORDER",
        "OTHERS","OUTER","OVER","PARTITION","PLAN","PRAGMA","PRECEDING",
        "PRIMARY","QUERY","RAISE","RANGE","RECURSIVE","REFERENCES",
        "REGEXP","REINDEX","RELEASE","RENAME","REPLACE","RESTRICT",
        "RIGHT","ROLLBACK","ROW","ROWS","SAVEPOINT","SELECT","SET",
        "TABLE","TEMP","TEMPORARY","THEN","TIES","TO","TRANSACTION",
        "TRIGGER","UNBOUNDED","UNION","UNIQUE","UPDATE","USING",
        "VACUUM","VALUES","VIEW","VIRTUAL","WHEN","WHERE","WINDOW",
        "WITH","WITHOUT"
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
ModelDefinition$methods(initialize=function(
    table="unknown",
    fields=list(),
    many=list(),
    one=list()
) {
    ## The regex in this methods never use the [A-Z] range because
    ## grepl must use the ignore.case systematically
    ## But, error messages must show the [A-Z] range in the regex
    ## to inform users they can use uppercasse.
    field_regex <- "^[a-z]+[a-z0-9_]+$"
    field_regex_error_message <- "^[A-Za-z]+[A-Za-z0-9_]+$"
    if (any(grepl(
        sprintf("^%s$", table), FORBIDEN_FIELDS, ignore.case=TRUE
    ))) {
        stop(paste("The table name", table, "is forbiden."))
    }
    if (!grepl(field_regex, table, perl=TRUE, , ignore.case=TRUE)) {
        stop("ModelDefinition$table must match", field_regex_error_message)
    }
    if (!is.list(fields)) {
        stop("ModelDefinition$fields must be a list of strings.")
    }
    if (!is.list(many)) {
        stop("ModelDefinition$many must be a list of strings.")
    }
    if (!is.list(one)) {
        stop("ModelDefinition$one must be a list of strings.")
    }
    if (any(names(fields) == "")) {
        stop("ModelDefinition$fields must have a name for each element.")
    }
    attributes <- list(fields=fields, many=many, one=one)
    for (name in names(attributes)) {
        kind <- attributes[[name]]
        for (field_name in names(attributes[[name]])) {
            field <- attributes[[name]][[field_name]]
            if (!is.character(field)) {
                stop(sprintf("ModelDefinition$%s must be a list of strings."))
            }
            if (!grepl(field_regex, field_name, perl=TRUE, ignore.case=TRUE)) {
                stop(sprintf(
                    "ModelDefinition$%s must contain names that match %s regex",
                    kind, field_regex_error_message
                ))
            }
            if (any(grepl(sprintf("^%s$", field_name), FORBIDEN_FIELDS))) {
                stop(sprintf(
                    "The field name %s is forbiden (%s model).", field_name, table
                ))
            }
        }
    }
    ## we set foreign keys as INTEGER fields named table_id
    .self$table <- table
    .self$fields <- fields
    .self$many <- many
    .self$one <- one
    .self$fields$id <- "INTEGER"
    for(field in one) {
        .self$fields[[paste0(field, "_id")]] <- "INTEGER"
    }
})
