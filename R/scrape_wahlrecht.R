replace_empty_with_na <- function(x){
  x <- x %>%
    stringr::str_replace_all(.,"^$|^ $","thisNA")
  x[x == "thisNA"] <- NA
  x
}


df_replace2<- function(df,pattern,replacement){
  df %>%
    mutate_all(funs(str_replace_all(string = .,
                                      pattern = pattern,
                                      replacement = replacement)))
}

df_replace_too_long <- function(df,length = 20){
  df %>%
    mutate_all(funs(ifelse(str_length(.) > length,NA,.)))
}


`%nin%` <- Negate(`%in%`)

extractGermanParties <- function(x,sonstige = F){

  y <- x %>% str_to_lower()
  if(sonstige == T) sel <- "(cdu/csu|cdu|csu|spd|grüne|linke|fdp|afd|pds|rep/dvu|sonstige)" else sel <- "(cdu/csu|cdu|csu|spd|grüne|linke|fdp|afd|pds|rep/dvu)"

  out_df <- x[str_detect(y,sel)]

  return(out_df)
}

#replace_empty_with_na(c("A",""," "))

remove_emptycols <- function(df){

  nonEmptyCols <-
    map_lgl(.x = 1:(ncol(df)),
            .f = function(x){
              (!all(df[,x] == "" | is.na(df[,x])))
            })

  clean_df <- df[,nonEmptyCols]

  return(clean_df)
}


## Parse Wahlrecht


parse_wahlrecht_url <- function(url){

  print(url)

  # read in url
  page <- xml2::read_html(url)

  raw <-
    page %>%
    rvest::html_nodes(css = ".wilko") %>%
    rvest::html_table(fill = TRUE) %>%
    .[[1]] %>%
    #.[-c(1:3),] %>%
    remove_emptycols

  if(colnames(raw)[1] == "X1")colnames(raw) <- raw[1,]

  colnames(raw)[1] <- "date"
  colnames(raw) <- colnames(raw) %>% str_to_lower()
  raw <- raw[,colnames(raw) != ""]

  raw <-
    raw %>%
    df_replace2(pattern = "– Als Umfragewert: nicht ausgewiesen; als Wahlergebnis: nicht teilgenommen",replacement = "") %>%
    df_replace_too_long(length = 30) %>%
    remove_emptycols %>%
    select(-one_of(c(".1",".2")))

  raw$year <- raw$date %>% as.Date("%d.%m.%Y") %>% lubridate::year()

  if("befragte" %nin% colnames(raw)) raw$befragte <- NA
  if("zeitraum" %nin% colnames(raw)) raw$zeitraum <- NA

  raw <-
    raw %>%
    filter(is.na(befragte)|befragte != "Bundestagswahl")

  # get institute
  raw$institute <-
    str_extract(url,'(allensbach|emnid|forsa|politbarometer|gms|dimap|insa|yougov)')

  raw$raw_by_institute <- str_detect(url,"stimmung")

  raw$participants <-
    raw$befragte %>%
    str_remove_all("[^\\d\\)]") %>% #remove everything but digits
    replace_empty_with_na(.) %>%
    as.numeric()

  parties <-
    colnames(raw) %>%
    extractGermanParties(sonstige = T)

  non_parties <- c("date","befragte","zeitraum","year","institute","participants","raw_by_institute")

  raw <-
    raw %>%
    mutate_at(vars(-one_of(non_parties)),
              funs(. %>%
                     str_remove_all(" %") %>%
                     str_replace_all(",",".") %>%
                     as.numeric() %>%
                     `/`(100)
                   )
    )

  # delete unnecessary columns

  # Enddate
  raw$enddate <-
    raw$zeitraum %>%
    str_extract(.,"(?<=\\–).+$")

  raw$startdate <-
    raw$zeitraum %>%
    str_extract(.,".+(?=\\–)")

# here maybe still with years

  raw <-
    raw %>%
    select(-befragte,
           -zeitraum)

  raw <-
    raw %>%
    filter(!(date %in% c("",NA))) %>%
    filter(!date %in% c("LINKE: DIE LINKE","Datum"))

  return(raw)
}



#' Scrape German Federal Level Polls from Wahlrecht.de
#' @description Scrape German Federal Election Polls from the website "Wahlrecht.de"
#' @param institutes A vector with institutes to include.
#' @param include_rawdata Logical whether raw data (available for some instutues) shall be included. Raw polls can be identified via the raw_by_institue column. Defaults to FALSE.
#' @param parties A character vector of which parties to include. Defaults to the main parties currently represented in the German Bundestag.
#' @return A data frame all polls since 1998 including Party Vote Shares along metadata including Year, Date of Publication, Start and End of Data Collection, Polling Institute, Sample Size and a "Raw"-Dummy.
#' @examples
#' scrape_wahlrecht_bund()
#' @section Notes:
#' The "other parties" column is calculated as the difference of the sum of all selected parties to 1.
#' @export
scrape_wahlrecht_bund <- function(institutes = c("allensbach",
                                                 "emnid",
                                                 "forsa",
                                                 "politbarometer",
                                                 "gms",
                                                 "dimap",
                                                 "insa",
                                                 "yougov"),
                                  include_rawdata = F,
                                  parties = c("cdu/csu",
                                              "spd",
                                              "grüne",
                                              "fdp",
                                              "linke",
                                              "afd")
                                  ){

  urls <- voteR::wahlrecht_urls_bund
  inst_selector <- paste0(institutes,collapse = "|")
  urls <- urls[str_detect(urls,inst_selector)]
  if(include_rawdata == F) urls[!str_detect(urls,"stimmung")]

  cat(crayon::blue("Starting Scraping from wahlrecht.de\n Thanks to the developers for providing the database.\n"))


  df <-
    map(.x = urls,
        .f = ~ parse_wahlrecht_url(.x)) %>%
    bind_rows()

  df <-
    df %>%
    mutate(linke = rowSums(select(., one_of("linke","linke.pds","pds","pds.linke")),na.rm = T))


  meta_vars <- c("date","year","institute","raw_by_institute","participants","enddate","startdate")

  out <-
    df %>%
    select(one_of(meta_vars,parties)) %>%
    mutate(others = 1 - rowSums(select(., one_of(parties)),na.rm = T))

  cat(crayon::green(paste0("Scraping finished. You have selected polls including ",crayon::red(paste0(parties,collapse = ", "))," from the institutes ",crayon::cyan(paste0(institutes,collapse = ", ")),". If using the data, please cite both the authors of wahlrecht.de as well as the API proived by voteR\n")))

  return(out)
}

#df <-  scrape_wahlrecht_bund()



