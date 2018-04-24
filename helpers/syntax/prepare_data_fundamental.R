library(plyr)
library(tidyverse)
library(magrittr)
library(stringr)

data("landtagswahlen")
data("bundestag_laenderebene")
data("laender")

data <- bind_rows(bundestag_laenderebene,
                  landtagswahlen)

# landtagswahlen %>% filter(land == "baden-wuerttemberg") %>% View()
# bundestag_laenderebene %>% filter(land == "baden-wuerttemberg") %>% View()
#
# data %>% filter(land == "baden-wuerttemberg") %>% View()


# Throw out all elections before reunification
#data %<>% filter(date >= "1990-12-02")

data2 <-
  data %>% gather(party,vote,cdu:others,-wbt) %>%
  mutate(partytype = ifelse(party %in% c("cdu","csu"),"union",ifelse(party %in% "spd","spd","small/other")))


# here stackoverflow problem
df <-
  data2 %>%
  arrange(land,party,date) %>%
  group_by(party,land,level) %>%
  mutate(lag_own = lag(vote,n = 1),
         distance_lag_own = as.numeric(date-lag(date,1)))

# %>%
#   ungroup () %>%
#   group_by(party,land) %>%
#   mutate(btw_t1 = lag(vote, n = 1),
#          distance_btw_t1 = as.numeric(date-lag(date,1))) %>%
#   filter(level == "landtagswahl") #%>%
#   filter(distance_btw_t1 >0)


result <- data.frame()

parties <- df$party %>% unique()
laenderr <- df$land%>% unique()

for(l in laenderr){
  for(p in parties){

    helper <- df %>% filter(party %in% p,land %in% l)

    helper$other <-
      with(helper, sapply(1:nrow(helper),
                    function(ind) {
                      gp_cur <- level[ind]
                      if(any(level[1:ind]!=gp_cur)) tail(vote[1:ind][level[1:ind]!=gp_cur], 1) else NA
                    }))

    helper$dateother <-
      with(helper, sapply(1:nrow(helper),
                          function(ind) {
                            gp_cur <- level[ind]
                            if(any(level[1:ind]!=gp_cur)) tail(as.character(date)[1:ind][level[1:ind]!=gp_cur], 1) else NA
                          }))

    result <- bind_rows(result,helper)
  }
}

result %<>% filter(level == "landtagswahl")


  # weiter

lreg <-
  read.csv2("helpers/rawdata/landesregierungen2018.csv",encoding = "UTF-8") %>%
  mutate_at(vars(Beteiligte.Parteien),
            funs(str_to_lower(.)))

lreg$X.U.FEFF.Land %>% table()

main_parties <- c("cdu","csu","spd","gruene","fdp","linke","afd","sonstige","other")


regpars<-
  lreg$Beteiligte.Parteien %>%
  str_replace_all("bündnis 90/die grünen","gruene") %>%
  str_replace_all("fdp/dps","fdp") %>%
  str_replace_all("fdp/dvp","fdp") %>%
  str_extract_all(paste0(main_parties,collapse = "|"), simplify = T)
colnames(regpars) <- paste0("party_",1:ncol(regpars))

lreg <-
  data.frame(lreg,regpars)

dauer <- lreg$Amtszeit.Jahr %>% str_split("-|–",simplify = T)
colnames(dauer) <- c("start","end")
dauer %<>% as.tibble () %>% mutate(end = ifelse(end == "",2018,end))

lreg <- data.frame(lreg,dauer)
names(lreg)
lreg %<>%
  dplyr::rename(land = X.U.FEFF.Land) %>%
  arrange(land,start)

result %>%
  filter(level == "landtagswahl") %>%
  ungroup () %>%
  select(land,year) %>% unique()

# hier nochmal checken ob das ganz sauber ist

getelection <-
  function(land = "baden-wuerttemberg", year = 2011) {

  elections <- landtagswahlen$year[landtagswahlen$land == land]
  elec <- elections [elections >= year] %>% .[1]
  if(identical(elec,character(0))) elec <- "none"

  return(elec)
  }


lreg$election <-
  map2(lreg$land,lreg$end,getelection) %>%
  unlist()

lreg2 <-
  lreg %>%
  group_by(land) %>%
  mutate(r = rank(election))

lreg$legislature <- lreg2$r


lastlregbeforeelection <-
  lreg %>%
  group_by(land,legislature) %>%
  slice(which.max(end))


xx <- left_join(result,
                lastlregbeforeelection,
                by = c("land",
                       "year" = "election")) %>%
  mutate(primeminister = party == party_1) %>%
  mutate_at(vars(contains("party")),funs(as.character(.)))

government <-
  pmap(.l = list(xx$party,xx$party_1,xx$party_2,xx$party_3),
       .f = function(x,y,z,a) x %in% c(y,z,a)) %>% unlist()

xx$gov <- government

xx <- xx %>%
  mutate(juniorpartner = ifelse(primeminister == TRUE,FALSE,gov))

####### BIP DATA

bip <- read.csv2("helpers/rawdata/bip.csv",stringsAsFactors = F) %>%
  mutate_at(vars(D),funs(str_replace(.,"\\.",""))) %>%
  mutate_all(funs(as.character(.))) %>%  mutate_all(funs(as.numeric(.)))

library(qdap)
remove_umlaut <- function(x){
  y <-
    x %>%
    str_replace_all("ü","ue") %>%
    str_replace_all("Ü","UE") %>%
    str_replace_all("ä","ae") %>%
    str_replace_all("Ä","AE") %>%
    str_replace_all("ö","oe") %>%
    str_replace_all("Ö","OE")
  return(y)
}

names(bip) <- mgsub(as.character(laender$shortlabel),as.character(laender$fullabel),names(bip)) %>%
  str_to_lower() %>% remove_umlaut() %>% str_replace_all("ï..jahr","year")

relbip <-
  bip %>%
  #mutate_at(vars(2:17),funs(./d)) %>%
  select(-d) %>%
  gather(-year,key = land,value = bip) %>%
  group_by(land) %>%
  mutate(biplag = (bip/lag(bip,2))-1) %>%
  as.data.frame()


datafinal <-
  xx %>% left_join(relbip %>% mutate_at(vars(year),funs(as.character(.))),by = c("land","year"))

datafinal$partytype %<>% as.factor() %>% relevel(ref = "union")

# assumption
datafinal %<>% mutate(lag_own = ifelse(is.na(lag_own),0.02,lag_own))

datafinal %<>%
  arrange(land,party,year) %>%
  mutate(firsttime = ifelse((!is.na(vote)) & is.na(lag(vote)),"Yes","No"))  %>%
  mutate(distance_other_lag = date - as.Date(dateother))

# look at this again
len <- length(datafinal$lag_own[datafinal$firsttime == "Yes"])
datafinal$lag_own[datafinal$firsttime == "Yes"] [sample(x = c(1:len),size = round(len/2))] <- 0.01

########

names(datafinal)
head(datafinal)

structural_modeldata <-
  datafinal %>%
  rename(lag_ltw = lag_own,
         lag_btw = other,
         bipchange = biplag,
         date_btw = dateother,
         cabinet = Kabinett,
         primeminister_name = Ministerpräsident,
         distance_btw_lag = distance_other_lag) %>%
  select(-distance_lag_own,-Amtszeit.Jahr,-Amtszeit.Datum,-Beteiligte.Parteien,-legislature,-level)%>%
  mutate_at(vars(distance_btw_lag),funs(as.numeric(.)))

names(structural_modeldata)
dim(structural_modeldata)


devtools::use_data(structural_modeldata,overwrite = TRUE)

landesregierungen <-
  lreg %>%
  rename(cabinet = Kabinett,
         years = Amtszeit.Jahr,
         dates = Amtszeit.Datum,
         parties = Beteiligte.Parteien,
         primeminister = Ministerpräsident
         ) %>%
  select(-election,-legislature) # remove

dim(landesregierungen)

devtools::use_data(landesregierungen,overwrite = TRUE)



# BIP

laenderbip <- bip
devtools::use_data(laenderbip,overwrite = TRUE)
