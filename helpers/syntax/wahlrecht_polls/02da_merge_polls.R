
polls_db <- dbConnect(RSQLite::SQLite(), "helpers/data/polldata/databases/polls.sqlite")

polls_bund <- dbReadTable(conn = polls_db,name = "polls_bundestag_bund")
polls_laender <- dbReadTable(conn = polls_db,name = "polls_landtagswahlen")

names(polls_bund)
names(polls_laender)
names(polls_bund) [!names(polls_bund) %in% names(polls_laender)]
names(polls_laender) [!names(polls_laender) %in% names(polls_bund)]


# N
polls_bund$poll_id %>% unique() %>% length()
polls_laender$poll_id %>% unique() %>% length()


polls_laender$befragte <- polls_laender$befragte_recoded


shared_variables <- Reduce(dplyr::intersect, list(names(polls_laender),
                                           names(polls_bund)))

polls_bund_sub <-
  polls_bund %>% select(one_of(shared_variables))

polls_laender_sub <-
  polls_laender %>% select(one_of(shared_variables))

polls_merged <-
  rbind.data.frame(polls_bund_sub,polls_laender_sub)

head(polls_merged)

# Fix elections
polls_merged$institut [polls_merged$is_poll == 0] <- "wahl"

# Unique
polls_merged <-
  polls_merged %>%
  distinct()

# data fixing
polls_merged$vote %<>% as.numeric()

important_parties <- c("afd","cdu","csu","cdu_csu","spd","fdp","gruene","linke_pds","linke","dielinke","piraten","sonstige","spd")

polls_merged$party %<>% as.character()

polls_merged %<>% dplyr::filter(party %in% important_parties)

polls_merged$year <-
  format(base::as.Date(polls_merged$datum_character, format="%Y-%m-%d"),"%Y") %>%
  as.numeric()

polls_merged$month <-
  format(base::as.Date(polls_merged$datum_character, format="%Y-%m-%d"),"%Y-%m")

polls_merged %<>% dplyr::filter(year >= 1998)

polls_merged$vote[polls_merged$bland == "BUND"] <- polls_merged$vote[polls_merged$bland == "BUND"]/100

polls_merged %<>% dplyr::filter(institut != "FG Wahlen")


# write to Database
polls_db <- dbConnect(RSQLite::SQLite(), "helpers/data/polldata/databases/polls.sqlite")
dbWriteTable(conn = polls_db,
             name = "polls_merged",
             value =  polls_merged,
             overwrite = TRUE)

dbListTables(conn = polls_db)

rm(polls_merged)
