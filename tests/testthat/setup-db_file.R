
source("./tests_configuration.R")

if(file.exists(DB_PATH)) {
    file.remove(DB_PATH)
}