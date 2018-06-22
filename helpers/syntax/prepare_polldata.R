polls_db <- dbConnect(RSQLite::SQLite(), "helpers/data/polldata/databases/polls.sqlite")
dbListTables(conn = polls_db)

german_polls <- dbReadTable(conn = polls_db,name = "polls_merged")

devtools::use_data(german_polls,overwrite = TRUE)
