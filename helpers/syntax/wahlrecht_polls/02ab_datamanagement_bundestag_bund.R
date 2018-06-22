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

# Formatierung des Ursprungsdatensatzes (1 Zeile = 1 Umfrage)

polls_db <- dbConnect(RSQLite::SQLite(), "helpers/data/polldata/databases/polls.sqlite")

polls_bb_RAW <- dbReadTable(conn = polls_db,name = "polls_bundestag_bund_RAW")

df <- tbl_df(polls_bb_RAW)

df <- df %>% select(datum, `CDU.CSU`:url, Linke.PDS, PIRATEN, PDS)

names(df) <- c('vdatum', 'cdu_csu', 'spd', 'gruene', 'fdp', 'linke', 'afd',
               'sonstige', 'befragte', 'feldzeit', 'institut', 'url',
               'piraten', 'linke_pds', 'pds')

df$poll_id <- c(1:nrow(df))

# df <- df %>% filter(!grepl('Wahl', vdatum) & !grepl('wahl', befragte) &
#                       !grepl('wahl', feldzeit))  # Wahlergebnisse entfernen

df <- df %>% mutate_at(vars(cdu_csu:sonstige, piraten:pds), str_replace,
                       pattern = ',', replacement = '.') %>%
  mutate_at(vars(cdu_csu:sonstige, piraten:pds), str_replace,
            pattern = ' %', replacement = '') %>%
  mutate_at(vars(cdu_csu:sonstige, piraten:pds), as.numeric)

df$linke_pds[which(is.na(df$linke_pds))] <- df$pds[which(is.na(df$linke_pds))]
df$pds <- NULL
df$linke_pds[which(is.na(df$linke_pds))] <- df$linke[which(is.na(df$linke_pds))]
df$linke <- NULL

df$befragte <-
  str_replace(df$befragte, '≈|>', '') %>%
  str_replace(pattern = '\\.', replacement = '') %>%
  str_replace(pattern = 'O • ', '') %>%
  str_replace(pattern = 'T • ', '')
df$befragte <- as.integer(df$befragte)

# Datumsangaben
df$vdatum_char <- df$vdatum
df$vdatum <- lubridate::dmy(df$vdatum)

df$feldzeit_beginn <- str_extract(df$feldzeit, '\\d\\d\\.\\d\\d\\.')
df$feldzeit_ende <- str_sub(df$feldzeit, -6, -1)

df$datum <-
  ifelse(month(df$vdatum) < as.integer(str_sub(df$feldzeit_ende, -3, -2)),
         paste0(df$feldzeit_ende, year(df$vdatum) - 1),
         paste0(df$feldzeit_ende, year(df$vdatum))
  ) %>% str_extract('\\d\\d\\.\\d\\d\\.\\d\\d\\d\\d') %>% dmy

df$datum[which(is.na(df$datum))] <- df$vdatum[which(is.na(df$datum))]

df$datum_char <- format(df$datum, '%d.%m.%Y')

df$jahr <- year(df$datum)

df$jahr[which(is.na(df$jahr))] <- year(df$vdatum[which(is.na(df$jahr))])

df$feldzeit_ende <- paste0(str_extract(df$feldzeit_ende, '\\d\\d\\.\\d\\d\\.'),
                           year(df$vdatum)) %>% dmy
df$feldzeit_beginn <- ifelse(dmy(paste0(str_extract(df$feldzeit_beginn,
                                                    '\\d\\d\\.\\d\\d\\.'),
                                        year(df$vdatum))) > df$feldzeit_ende,
                             dmy(paste0(str_extract(df$feldzeit_beginn,
                                                    '\\d\\d\\.\\d\\d\\.'),
                                        year(df$vdatum) - 1)),
                             dmy(paste0(str_extract(df$feldzeit_beginn,
                                                    '\\d\\d\\.\\d\\d\\.'),
                                        year(df$vdatum))))


# Institutsnamen anpassen
df$institut <- car::recode(df$institut, "'allensbach' = 'Allensbach';
                           'emnid' = 'Emnid'; 'forsa' = 'Forsa';
                           'politbarometer' = 'FG Wahlen'; 'gms' = 'GMS';
                           'dimap' = 'Infratest Dimap'; 'insa' = 'INSA'")

# Longfrom (i.e. tidy data): 1 Zeile = 1 Partei in einer Umfrage
df <- df %>% tidyr::gather(key = party, value = vote,
                           cdu_csu:sonstige, piraten:linke_pds, -befragte)

df <- df %>% dplyr::filter(party != 'sonstige', party != 'piraten')

df$bland <- "BUND"

df$is_bundestagswahl <- ifelse(is.na(df$feldzeit) | df$feldzeit != "Bundestagswahl",0,1)

df$is_poll <- ifelse(is.na(df$feldzeit) | df$feldzeit != "Bundestagswahl",1,0)

df$datum_character <- as.character(df$vdatum)


# write to Database
polls_db <- dbConnect(RSQLite::SQLite(), "helpers/data/polldata/databases/polls.sqlite")
dbWriteTable(conn = polls_db,
             name = "polls_bundestag_bund",
             value =  df,
             overwrite = TRUE)

dbListTables(conn = polls_db)

rm(df)
