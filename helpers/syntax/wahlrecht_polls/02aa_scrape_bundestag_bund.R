# Downloads raw htmls, then saves them as raw csv


download_bundespolls_bund <-
  function (firsttime = FALSE){

    urls <- read.delim('helpers/data/polldata/list_urls_bund.txt',stringsAsFactors = F,header = F)$V1
    d_new <- data.frame()

    for (url in urls){

      filename <- paste0(str_replace(str_sub(url,34,-5),'/','-'))

      #Redownload if either file does not exist or recent data
      if ((! paste0(filename,".html") %in% list.files("helpers/data/polldata/bund/raw_polls_html/")) | (!grepl("[[:digit:]]", filename))| firsttime == TRUE){
        download.file(url, destfile = paste0('helpers/data/polldata/bund/raw_polls_html/',filename,".html"), quiet=TRUE)
        cat(filename)
      }

      tmp <- htmltab(paste0('helpers/data/polldata/bund/raw_polls_html/',filename,".html"),
                     which = 2)
      names(tmp)[1] <- 'datum'

      tmp$institut <-
        str_extract(url,'(allensbach|emnid|forsa|politbarometer|gms|dimap|insa)')

      tmp$url <- url

      if ((! paste0(filename,".csv") %in% list.files("helpers/data/polldata/bund/raw_polls/")) | (!grepl("[[:digit:]]", filename))| firsttime == TRUE){
        write.csv2(tmp,file=paste0('helpers/data/polldata/bund/raw_polls/',filename,'.csv'),row.names=F)
      }

      d_new <- bind_rows(d_new, tmp)
      d_new$timestamp <- Sys.time()  # timestamp

    }
    d_new <- d_new %>% distinct(.keep_all = TRUE)
    d_new
  }

df_polls_bb_raw <-
  download_bundespolls_bund(firsttime = FALSE) # put TRUE if using code for the first tiem


polls_db <- dbConnect(RSQLite::SQLite(), "helpers/data/polldata/databases/polls.sqlite")
dbWriteTable(conn = polls_db,
             name = "polls_bundestag_bund_RAW",
             value =  df_polls_bb_raw,
             overwrite = TRUE)

dbListTables(polls_db)

rm(df_polls_bb_raw)

