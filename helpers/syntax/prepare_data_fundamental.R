library(tidyverse)
library(magrittr)
library(stringr)

impose_lag <- 0.00

data("landtagswahlen")
data("landtagswahlen_mitRechts")

data("bundestag_laenderebene_mitRechts")
data("laender")

data <- bind_rows(bundestag_laenderebene_mitRechts,
                  landtagswahlen_mitRechts)

names(data)
new2018 <- data.frame(year = c("2018","2018"),
                      land = c("bayern","hessen"),
                      date = c(as.Date("2018-10-14"),as.Date("2018-10-28")),
                      cdu = c(NA,0.27),
                      csu = c(0.372,NA),
                      spd = c(0.097,0.198),
                      fdp = c(0.051,0.075),
                      gruene = c(0.176,0.198),
                      linke = c(0.032,0.063),
                      afd = c(0.102,0.131),
                      rep = c(NA,NA),
                      npd = c(NA,0.002),
                      wbt = c(0.732,0.673),
                      level = c("landtagswahl","landtagswahl")) %>%
  mutate(others = 1-rowSums(select(.,cdu,csu,spd,fdp,gruene,linke,afd,rep,npd),na.rm =T))

data <- bind_rows(data,new2018)

data$rightwing <- rowSums(data.frame(data$npd,data$afd,data$rep),na.rm = T)

# landtagswahlen %>% filter(land == "baden-wuerttemberg") %>% View()
# bundestag_laenderebene %>% filter(land == "baden-wuerttemberg") %>% View()
#
# data %>% filter(land == "baden-wuerttemberg") %>% View()


# Throw out all elections before reunification
#data %<>% filter(date >= "1990-12-02")

data2 <-
  data %>% gather(party,vote,cdu:others,rightwing,-wbt) %>%
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


df <-
  df %>%
  mutate(t_365 = as.Date(date) - as.difftime(365,unit = "days"),
         t_182 = as.Date(date) - as.difftime(182,unit = "days"),
         t_91  = as.Date(date) - as.difftime(91,unit = "days"),
         t_30 = as.Date(date) - as.difftime(30,unit = "days"),
         t_7 = as.Date(date) - as.difftime(7,unit = "days"),
         t_0 = as.Date(date)) %>%
  gather(lag,lagdate,t_365:t_0) %>%
  mutate_at(vars(lag),funs(as.numeric(str_remove_all(.,"t_"))))



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

    helper$other2 <-
      with(helper, sapply(1:nrow(helper),
                          function(ind) {
                            gp_cur <- level[ind]
                            if(any(level[1:ind]!=gp_cur)) tail(vote[1:ind][level[1:ind]!=gp_cur], 2)[1] else NA
                          }))

    helper$dateother <-
      with(helper, sapply(1:nrow(helper),
                          function(ind) {
                            gp_cur <- level[ind]
                            if(any(level[1:ind]!=gp_cur)) tail(as.character(date)[1:ind][level[1:ind]!=gp_cur], 1) else NA
                          }))

    helper$dateother2 <-
      with(helper, sapply(1:nrow(helper),
                          function(ind) {
                            gp_cur <- level[ind]
                            if(any(level[1:ind]!=gp_cur)) tail(as.character(date)[1:ind][level[1:ind]!=gp_cur], 2)[1] else NA
                          }))

    result <- bind_rows(result,helper)

  }
}

result %<>% filter(level == "landtagswahl")

result <-
  result %>%
  mutate(otherdate = ifelse(lagdate > dateother,dateother,ifelse(lagdate > dateother2,dateother2,NA)),
         othervalue = ifelse(lagdate > dateother,other,ifelse(lagdate > dateother2,other2,NA))) %>%
  select(-c(other:dateother2))


  # weiter

lreg <-
  read.csv2("helpers/rawdata/landesregierungen2018.csv",encoding = "UTF-8") %>%
  mutate_at(vars(Beteiligte.Parteien),
            funs(str_to_lower(.)))

lreg$X.U.FEFF.Land %>% table()

main_parties <- c("cdu","csu","spd","gruene","fdp","linke","afd","npd","rep","sonstige","other")


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

bip <- read.csv2("helpers/rawdata/bip2018.csv",stringsAsFactors = F,encoding = "UTF-8") %>%
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

laender$fullabel <- laender$fullabel %>% str_replace_all("Baden-WÃ¼rttemberg","Baden-Württemberg") %>% str_replace_all("ThÃ¼ringen","Thüringen")

names(bip) <- mgsub(as.character(laender$shortlabel),str_replace_all(as.character(laender$fullabel),"ü","ue"),names(bip)) %>%
  str_to_lower() %>% remove_umlaut() %>% str_replace_all("x.u.feff.jahr","year")

relbip <-
  bip %>%
  #mutate_at(vars(2:17),funs(./d)) %>%
  select(-d) %>%
  gather(-year,key = land,value = bip) %>%
  group_by(land) %>%
  mutate(lag45 = (lag(bip,4)/lag(bip,5))-1,
         lag34 = (lag(bip,3)/lag(bip,4))-1,
         lag23 = (lag(bip,2)/lag(bip,3))-1,
         lag12 = (lag(bip,1)/lag(bip,2))-1,
         lag01 = (bip/lag(bip,1))-1) %>%
  as.data.frame()
relbip$biplag = rowMeans(relbip[, c("lag45","lag34","lag23","lag12")],na.rm = T)
relbip$biplag[is.nan(relbip$biplag)] <- NA
relbip$biplag2 <- relbip$lag12

datafinal <-
  xx %>% left_join(relbip %>% mutate_at(vars(year),funs(as.character(.))),by = c("land","year"))

datafinal$partytype %<>% as.factor() %>% relevel(ref = "union")

# assumption
datafinal %<>% mutate(lag_own = ifelse(is.na(lag_own),impose_lag,lag_own))

datafinal %<>%
  arrange(land,party,year) %>%
  group_by(lag) %>%
  mutate(firsttime = ifelse((!is.na(vote)) & is.na(lag(vote)),"Yes","No"))  %>%
  ungroup() %>%
  mutate(distance_other_lag = date - as.Date(otherdate))


########

names(datafinal)
head(datafinal)

structural_modeldata_long <-
  datafinal %>%
  rename(lag_ltw = lag_own,
         lag_btw = othervalue,
         bipchange_avg = biplag,
         bipchange = biplag2,
         date_btw = otherdate,
         cabinet = Kabinett,
         primeminister_name = Ministerpräsident,
         distance_btw_lag = distance_other_lag) %>%
  select(-distance_lag_own,-Amtszeit.Jahr,-Amtszeit.Datum,-Beteiligte.Parteien,-legislature,-level)%>%
  mutate_at(vars(distance_btw_lag),funs(as.numeric(.))) %>%
  mutate(rightwingdummy = ifelse(party %in% c("afd","npd","rep"),1,0))

names(structural_modeldata_long)
dim(structural_modeldata_long)

devtools::use_data(structural_modeldata_long,overwrite = TRUE)

saveRDS(structural_modeldata_long,file = "C:/Users/MS/OneDrive/github/paper/thesis/data/final/strData_final.rds")



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

