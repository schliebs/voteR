# Funktionen Basics

data(laender)


`%without%` <- function(x, o) x<-x[!(x %in% o)] # returns vector without certain specified elements
`%nin%` <- function(x, o) return(!(x %in% o)) # returns opposite of %in%
`%=%` <- function(vector,num) return(sapply(vector,all.equal,num)==T) #

`%nin%` <- function(x, o) return(!(x %in% o))

require(tidyverse)
require(stringr)
require(magrittr)

fill_with_last <- function(x,last = NA){for(i in seq_along(x)) if(x[i] == "" & last!='Name')x[i] <- last else last <- x[i];x} # fill blanks

getYear<-function(x){if (as.numeric(x)<40){return(as.numeric(paste0('20',x)))}else{return(as.numeric(paste0('19',x)))}} # Achtung ab 2040 wenn wir fame sind muessen wir hier anpassen :*

setZeros<-function(x){a<-x;a[a=='']<-'0';return(as.numeric(a))}

rawpath <- 'data/offline/results_bund_RAW/'
result <- data.frame()

for (fl in list.files(rawpath)){

  # ACHTUNG HIER FIXEN NUTZERBEZOGEN
 if(fl != "btw17_kerg.csv") df_elec <- read.csv2(file(paste0(rawpath,fl)),stringsAsFactors = F,header = F,skip = 5) else  df_elec <- read.csv2(file(paste0(rawpath,fl)),stringsAsFactors = F,header = F,skip = 5,encoding = "UTF-8")

  df_elec <- df_elec[,apply(df_elec,2,function(x){sum(!is.na(x))>0})] # Deletes Columns with only NA's

  df_elec[1,] <- fill_with_last(df_elec[1,],last = "")

  if (apply(df_elec,1,function(x){sum(x!='',na.rm=T)})[2]>5) df_elec[2,] <- fill_with_last(df_elec[2,],last = "")

  delete_row<- -1

  for (ci in colnames(df_elec)[-1:-3]){
    if (sum(str_detect(df_elec[,ci],'Vorperiode'))==1){delete_row<-(1:nrow(df_elec))[str_detect(df_elec[,ci],'Vorperiode')];df_elec[,ci]<-NULL}
  }

  first<-df_elec[2,]=='Erststimmen'
  second<-df_elec[2,]=='Zweitstimmen'
  if (delete_row>0) df_elec<-df_elec[-delete_row,]

  colnames(df_elec)<-
    apply(df_elec[1:2,],2,function(x){paste(x,collapse = ' ')}) %>%
    str_replace_all("Christlich Demokratische Union Deutschlands","cdu") %>%
    str_replace_all("Christlich-Soziale Union in Bayern e.V.","csu") %>%
    str_replace_all("Sozialdemokratische Partei Deutschlands","spd") %>%
    str_replace_all("BÜNDNIS 90/DIE GRÜNEN","gruene") %>%
    str_replace_all("Freie Demokratische Partei","fdp") %>%
    str_replace_all("Alternative für Deutschland","afd") %>%
    str_replace_all("DIE LINKE","linke") %>%
    str_to_lower() %>%        #lower habe ich vorgezogen
    str_replace('erststimmen','') %>%
    str_replace('zweitstimmen','') %>%
    str_replace_all("ä","ae") %>%
    str_replace_all("ö","oe") %>%
    str_replace_all("ü","ue") %>%
    str_replace_all('violetten','dievioletten') %>%
    str_replace_all('b90gr','gruene') %>%
    str_replace_all('dietierschutzpartei','tierschutz') %>%
    str_replace_all('tierschutzpartei','tierschutz') %>%
    str_replace_all('pds','linke') %>%
    str_replace_all('[\\-\\./ !\\%]','')


  colnames(df_elec)[1:3]<-c('WKID','WKN','WKP')

  df_elec$WKP<-str_to_upper(df_elec$WKP)
  df_elec$WKID<-as.numeric(as.character(df_elec$WKID))

  df_elec<-df_elec[-1:-2,]

  df_elec$WKN<-
    df_elec$WKN %>%
    str_replace_all("ä","ae") %>%
    str_replace_all("ö","oe") %>%
    str_replace_all("ü","ue") %>%
    str_replace_all('\\-',' ') #%>%
    #str_replace_all('[ Â]+',' ')



  wb <- which(colnames(df_elec) %in% c("wahlberechtigte"))
  wae <- which(colnames(df_elec) %in% c("waehler"))
  if(length(wb)>1) df_elec[,wb[2]] <- df_elec[,wb[1]]
  if(length(wae)>1) df_elec[,wae[2]] <- df_elec[,wae[1]]


  for (ci in 4:ncol(df_elec)) df_elec[,ci] %<>% setZeros()

  df_elec<-df_elec[apply(df_elec[,-1:-3],1,function(x){sum(x,na.rm=T)})>0,]


  if (sum(second)>0) first_df <- df_elec[,!second] else first_df <- data.frame()

  second_df<-df_elec[,!first]


  yy <- fl %>% str_sub(4,5)

  result_add <- bind_rows(first_df %>% mutate(type = "first"),
                      second_df %>% mutate(type = "second")) %>%
    mutate(year = ifelse(yy < 40, paste0("20",yy),paste0("19",yy)))

  result <- bind_rows(result,result_add)

}


result %<>%
  select(one_of(c("WKID","WKN","WKP",
                  "wahlberechtigte","waehler",
                  "ungueltige","gueltige",
                  "year","type",names(partynames$germany))))

# Dranmatchen des landes

dates <- read.csv2(file = "helpers/rawdata/dates.csv") %>% mutate_if(is.integer,as.character)


result %<>% mutate_at(vars(WKP),
                      funs(WKP = ifelse(str_length(WKP) == 1 & str_sub(WKP,-1,-1) %in% 0:9,
                                            paste0("90",WKP),
                                            ifelse(str_length(WKP) == 2& str_sub(WKP,-1,-1) %in% 0:9,
                                                   paste0("9",WKP),
                                                   WKP))))


matches <-
  result %>%
  filter(WKID > 900 | WKP == 999 | str_detect(WKID,"[A-Z]")) %>%
  mutate_at(vars(WKID),funs(as.character(.))) %>%
  select(land = WKN,idd = WKID,year) %>%
  unique() %>%
  mutate_at(vars(idd),
            funs(idd = ifelse(str_length(idd) == 1 & str_sub(idd,-1,-1) %in% 0:9,
                              paste0("90",idd),
                              ifelse(str_length(idd) == 2& str_sub(idd,-1,-1) %in% 0:9,
                                     paste0("9",idd),
                                     idd))))

final_UPTO94 <-
  result %>%
  filter(year  <= 1994) %>%
  filter(WKID < 900,WKP != 999, WKN != "Bundesgebiet") %>%
  left_join(laender %>% select(land = id,shortlabel),
            by = c("WKP" = "shortlabel"))


final_STARTING98 <-
    result %>%
    filter(year  >= 1998)  %>%
    filter(WKID < 900,WKP != 999, WKN != "Bundesgebiet") %>%
    left_join(matches,
              by = c("WKP" = "idd","year")) %>%
  mutate_at(vars(land),funs(str_to_lower(str_replace_all(.," ","-"))))


final <-
  bind_rows(final_UPTO94,
            final_STARTING98) %>%
  left_join(dates,by = "year")


allvars <- c("date","year","land","type","WKID","WKN","WKP","wahlberechtigte","waehler","ungueltige","gueltige")
main_parties <- c("cdu","csu","spd","fdp","gruene","fdp","linke","afd")

final$land %<>% str_replace_all("baden-wuettemberg","baden-wuerttemberg")

bundestag_laenderebene <-
  final %>%
  filter (type == "second") %>%
  select(one_of(allvars,main_parties)) %>%
  group_by(year,land,date) %>%
  summarise_at(vars(one_of("wahlberechtigte", "waehler","ungueltige","gueltige",main_parties)),funs(sum(.))) %>%
  ungroup() %>%
  mutate_at(vars(one_of(main_parties)),
            funs(./gueltige)) %>%
  mutate(wbt = waehler/wahlberechtigte) %>%
  mutate(others = 1- rowSums(select(.,one_of(main_parties)),na.rm = T)) %>%
  select(-wahlberechtigte,-waehler,-gueltige,-ungueltige) %>%
  mutate_at(vars(date),funs(as.Date(.,format = "%d.%m.%Y"))) %>%
  mutate(level = "bundestagswahl")

devtools::use_data(bundestag_laenderebene,overwrite = TRUE)

write.csv2(bundestag_laenderebene,"helpers/data/bundestag_all.csv")
