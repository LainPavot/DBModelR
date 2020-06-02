

if ("DBModelR" %in% rownames(installed.packages())) {
    remove.packages("DBModelR")
}
install.packages(".", repos=NULL, type="source")
