# Scrape Bundespolls

library(conflicted)
install.load::install_load("plyr",
                           "tidyverse",
                           "magrittr",
                           "haven",
                           "DBI",
                           "RSQLite",
                           "ggplot2",
                           "hrbrthemes",
                           "extrafont",
                           "stringr",
                           "rvest",
                           "lubridate",
                           "htmltab","purrr",
                           "rvest",
                           "magrittr",
                           "tidyr",
                           "zoo")

# Update Bundestagspolls
source("helpers/syntax/wahlrecht_polls/02aa_scrape_bundestag_bund.R")
source("helpers/syntax/wahlrecht_polls/02ab_datamanagement_bundestag_bund.R")

# Update Bundestagspolls auf Länderebene
## NOT YET IMPLEMENTED (workingly)


# Update Landtagspolls
source("helpers/syntax/wahlrecht_polls/02ca_scrape_landtagswahlen.R")

# Merge
source("helpers/syntax/wahlrecht_polls/02da_merge_polls.R")

###
polls_db <- dbConnect(RSQLite::SQLite(), "helpers/data/polldata/databases/polls.sqlite")
dbListTables(conn = polls_db)

polls_final <- dbReadTable(conn = polls_db,name = "polls_merged")

