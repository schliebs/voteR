

# inspired by: https://raw.githubusercontent.com/cutterkom/germanpolls/master/R/scrape_wahlrechtde.R

# Different scraping variables
institute <- c("allensbach", "emnid", "forsa", "politbarometer", "gms", "dimap", "insa")
parteien <- c("cdu","csu","cdu/csu", "spd", "grüne", "fdp", "linke", "afd", "sonstige", "nw_un", "fw")
header <- c("CDU/CSU", "SPD", "GRÜNE", "FDP", "LINKE", "AfD", "Sonstige", "Nichtwähler/Unentschl.", "FW", "Befragte", "Zeitraum")
blaender <- c("baden-wuerttemberg","bayern","berlin","brandenburg","bremen","hamburg","hessen","mecklenburg-vorpommern",
              "niedersachsen","nordrhein-westfalen","rheinland-pfalz","saarland","sachsen","sachsen-anhalt","schleswig-holstein","thueringen")

# Overview Table
url <- "http://www.wahlrecht.de/umfragen/landtage/"
page <- read_html(url, encoding = "utf-8")

table <- html_node(page, ".wilko")
overview_df <- html_table(table,fill = TRUE)[-c(1:2),]


# Specific Länder

download_landtagspolls <- function() {

  joint_df <- data.frame()

  for (l in blaender){

    print(l)

  url2 <- paste0("http://www.wahlrecht.de/umfragen/landtage/",l,".htm")
  page <- read_html(url2, encoding = "utf-8")

  table <- html_nodes(page, ".wilko")
  table_df <- html_table(table,fill = TRUE)
  table_all <-  bind_rows(table_df)


  table_all$is_poll <-  !(str_detect(string =table_all$Institut,
                                     pattern = paste(c("Institut",
                                                       "Als Umfragewert",
                                                       "Telefon",
                                                       "AfD",
                                                       "CDU"),collapse = '|')))

  table_all$is_landtagswahl <-  str_detect(string = table_all$Institut,
                                           pattern = paste(c("Landtagswahl","landtagswahl"),collapse = '|'))


  table_cleaned <- table_all[table_all$is_poll,]

  # clean data
  names(table_cleaned) %<>% tolower()

  # Delete empty columns:

  table_cleaned <- table_cleaned[,!is.na(colnames(table_cleaned)) & colnames(table_cleaned)!= "" &
                                   colSums(is.na(table_cleaned)|table_cleaned == "")<nrow(table_cleaned)]

  ##
  table_cleaned %>% names()

  table_cleaned <-
    table_cleaned %>%
    dplyr::mutate(typ = ifelse(
      grepl("O ", befragte), "online",
      ifelse(grepl("T ", befragte), "phone", "no_info")))

  # gsub
  table_cleaned$befragte <- gsub(table_cleaned$befragte , pattern = "[^[:alnum:][:punct:][:space:]]", replacement = "")

  table_cleaned <- table_cleaned %>% map(gsub, pattern = ",", replacement = ".") %>% as.data.frame()

  table_cleaned %<>% map_at(parteien, gsub,pattern = "%",replacement = "") %>% as.data.frame()

  table_cleaned$datum <- str_replace_all(table_cleaned$datum, pattern = "Landtagswahl am ","")
  table_cleaned$datum <- base::as.Date(table_cleaned$datum, "%d.%m.%Y")

  table_cleaned$zeitraum <- gsub(table_cleaned$befragte , pattern = "[^[:digit:]]", replacement = "") %>% str_sub(start = -8)
  table_cleaned$zeitraum [table_cleaned$is_landtagswahl== TRUE] <- NA

  table_cleaned$befragte_recoded <- gsub(table_cleaned$befragte , pattern = "[^[:digit:]]", replacement = "") %>% str_sub(end = -9)
  table_cleaned$befragte_recoded [table_cleaned$is_landtagswahl== TRUE] <- NA

  names(table_cleaned)


  # Fix umlaute
  to.plain <- function(s) {

    # 1 character substitutions
    # old1 <- "šžþàáâãäåçèéêëìíîïðñòóôõöùúûüý"
    # new1 <- "szyaaaaaaceeeeiiiidnooooouuuuy"
    # s1 <- chartr(old1, new1, s)

    # 2 character substitutions
    old2 <- c("ö", "ß", "ä", "ü","Ö","Ä","Ü")
    new2 <- c("oe", "ss", "ae", "ue","oe","ae","ue")
    s2 <- s
    for(i in seq_along(old2)) s2 <- gsub(old2[i], new2[i], s2, fixed = TRUE)

    s2
  }
  table_cleaned[] <- lapply(table_cleaned, to.plain)
  table_cleaned

  colnames(table_cleaned) <- to.plain(colnames(table_cleaned))


  # Fix sonstige

  # save <- table_cleaned
  # table_cleaned <- save
  #


  table_cleaned$sonstige %<>% as.character()

  # REMOVE Sonstige with more than 50 Characters length (most likely zusatzrow)

  table_cleaned <- table_cleaned[!(str_length(table_cleaned$sonstige)>50),]

  #table_cleaned <- table_cleaned[!(str_detect(table_cleaned$sonstige,"Die Werte der")),]
  #table_cleaned <- table_cleaned[!(str_detect(table_cleaned$sonstige,"GRÜNE: Kurzbezeichnung")),]
  #table_cleaned <- table_cleaned[!(str_detect(table_cleaned$sonstige,"LINKE: Zahlen")),]


  # Manual fixes: Berlin abgeordnetenwahl 2006
  #table_cleaned$sonstige <- str_replace_all(table_cleaned$sonstige,"NPD 2.6Sonst. 4.4","NPD 2.6 Sonst. 4.4")


  # Remove empty first row
  table_cleaned <- table_cleaned[rowSums(is.na(table_cleaned)) != ncol(table_cleaned),]

  #table_cleaned$sonstige[which(str_detect(table_cleaned$sonstige,"NPD/DVU/REP"))] <- "NPD_DVU_REP 2.1 Sonst. 3.0"


  # Fix Bü'90 (achtung: replace umlaute before that)
  #table_cleaned$sonstige %<>%  iconv(x, "latin1", "ASCII", sub="")

  table_cleaned$sonstige %<>% gsub("[^\x20-\x7E]", "", .)

  # Fix sonderfall Bue'90
  table_cleaned$sonstige %<>% gsub("Bue90", "buendnisneunzig", .)

  # Remove dots
  table_cleaned$sonstige %<>% gsub("([A-z])([.])", "\\1", .)

  # Remove dots before
  table_cleaned$sonstige %<>% gsub("([.])([A-z])", "\\2", .)

  # Remove dashes
  table_cleaned$sonstige %<>% gsub("([A-z])-([A-z])", "\\1_\\2", .)

  # Remove lone points
  table_cleaned$sonstige %<>% gsub(" ([.]) ", " ", .)

  # Fix space if character follows number
  table_cleaned$sonstige %<>% gsub("([0-9])([A-z])", "\\1 \\2", .)



  # Fix if number follows character
  table_cleaned$sonstige %<>% gsub("([A-z])([0-9])", "\\1 \\2", .)


  # Replace two-parties sep. by /
  table_cleaned$sonstige %<>% gsub("([A-z])/([A-z])", "\\1_\\2", .)

  # Replace two-parties sep. by +
  table_cleaned$sonstige %<>% gsub("([A-z])\\+([A-z])", "\\1_\\2", .)

  # Replace Two-word party names
  table_cleaned$sonstige %<>% gsub("([A-z]) ([A-z])", "\\1_\\2", .)





  # Fix ?
  table_cleaned$sonstige %<>% gsub("([\\?])([A-z])", "NA \\2", .)
  table_cleaned$sonstige %<>% gsub("([\\?])", "NA", .)


  table_cleaned$sonstige <- str_replace_all(table_cleaned$sonstige,"\n","")
  #table_cleaned$sonstige <- str_replace_all(table_cleaned$sonstige,"Neue Lib.","Neue_Lib.")
  #table_cleaned$sonstige <- str_replace_all(table_cleaned$sonstige,"Scheuerl-Partei","Scheuerl_Partei 0") ##???
  #table_cleaned$sonstige <- str_replace_all(table_cleaned$sonstige,"Heimat HH","Heimat_HH") # ergebnisse dieser checken
  #table_cleaned$sonstige <- str_replace_all(table_cleaned$sonstige,"Schill-Partei","Schill_Partei")
  #table_cleaned$sonstige <- str_replace_all(table_cleaned$sonstige,"Pro DM Schill","Pro_DM_Schill")

  # bremen 126
  #table_cleaned$sonstige <- str_replace_all(table_cleaned$sonstige,"Regenbogen 2 REP 2 . DVU 2 Sonst. 1","Regenbogen 2 REP 2 DVU 2 Sonst. 1")

  #136
  #table_cleaned$sonstige <- str_replace_all(table_cleaned$sonstige,".Statt","Statt")
  #table_cleaned$sonstige <- str_replace_all(table_cleaned$sonstige,".PDS","PDS")
  #table_cleaned$sonstige <- str_replace_all(table_cleaned$sonstige,". Sonst.","Sonst.")


  others <-
    table_cleaned$sonstige %>% map(str_split,pattern = " ") %>% map(as.matrix,ncol = 2)


  other_df <- data.frame(row1 = 1:length(others))
  other_df$sonstige_together <- NA

  for (q in 1:length(others)){
    print(q)

    if(str_length(table_cleaned$sonstige[q])<=5){other_df$sonstige_together [q] <- table_cleaned$sonstige[q]}
    if(str_length(table_cleaned$sonstige[q])>5){

      m <-  others[[q]][[1]]

      # Bremen 11
      m <- m[m!=""]


      if (1 == length(m)%%2){
        m2 <- (m[-length(m)] %>% matrix(ncol=2,byrow= TRUE))}

      if (0 == length(m)%%2){
        m2 <-  (m %>% matrix(ncol=2,byrow= TRUE))
      }

      other_df$sonstige_together[q] <- sum(as.numeric(m2[,2]),na.rm = TRUE)

      for (r in 1:nrow(m2)){

        if((! m2[r,1] %in% colnames(other_df)) ) {
          eval(parse(text=paste0("other_df$",m2[r,1]," <- NA")))
          eval(parse(text=paste0("other_df$",m2[r,1]," [",q,"] <- ",ifelse(m2[r,2]=="?",NA,m2[r,2]),"")))
        } else

          if( m2[r,1] %in% colnames(other_df)) {
            eval(parse(text=paste0("other_df$",m2[r,1]," [",q,"] <- ",ifelse(m2[r,2]=="?",NA,m2[r,2]),"")))
          } else
            if (!m2[r,1] %in% colnames(table_cleaned) ) {
              eval(parse(text=paste0("table_cleaned$",m2[r,1]," [",q,"] <- ",ifelse(m2[r,2]=="?",NA,m2[r,2]),"")))
            }

      }
    }
  }

  table_cleaned <- cbind.data.frame(table_cleaned,other_df[,-1])

  table_cleaned$poll_id <- paste0(l,"-",c(1:nrow(table_cleaned)))
  # transform data set to longform
  table_long <- table_cleaned %>%
    dplyr::mutate(id = 1:nrow(.)) %>%
    gather(party, vote, -id,-institut, -poll_id,-auftraggeber,-is_poll,-is_landtagswahl,-typ,-datum, -befragte,-befragte_recoded,-zeitraum)

  table_long$vote <- as.numeric(table_long$vote)/100

  table_long$bland <- l

  # join em together
  joint_df <- bind_rows(joint_df,table_long)

  }

  # all strings to lower
  joint_df2 <- joint_df
  joint_df2[] <- lapply(joint_df2, str_to_lower)
  joint_df2
}


###########################################

df_polls_landtagswahlen <-
  download_landtagspolls()

df_polls_landtagswahlen$is_poll <- ifelse(df_polls_landtagswahlen$is_landtagswahl,0,1)

df_polls_landtagswahlen$datum_character <-
  as.character(df_polls_landtagswahlen$datum)


#df_polls_cc_raw$id %>% unique() %>% length()

polls_db <- dbConnect(RSQLite::SQLite(), "helpers/data/polldata/databases/polls.sqlite")
dbWriteTable(conn = polls_db,
             name = "polls_landtagswahlen",
             value =  df_polls_landtagswahlen,
             overwrite = TRUE)

dbListTables(polls_db)

rm(df_polls_landtagswahlen)
