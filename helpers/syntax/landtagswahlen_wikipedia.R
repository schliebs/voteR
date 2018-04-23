#
library(tidyverse)
library(rvest)
library(magrittr)



url <- "https://de.wikipedia.org/wiki/Ergebnisse_der_Landtagswahlen_in_der_Bundesrepublik_Deutschland"

#xml2::download_html(url,file = "landtagswahlen_wiki.html")
brd <- read_html("landtagswahlen_wiki.html")


spans <- brd %>%
  html_nodes(xpath = "//*/tr/td/span")
xml_remove(spans)


# BW

badenwuerttemberg_vote <-
  brd %>%
  html_nodes(xpath = '//*[@id="mw-content-text"]/div/table[4]') %>%
  html_table() %>%
  .[[1]] %>%
  mutate(land = "baden-wuerttemberg")

badenwuerttemberg_seats <-
  brd %>%
  html_nodes(xpath = '//*[@id="mw-content-text"]/div/table[5]') %>%
  html_table() %>%
  .[[1]] %>%
  mutate(land = "baden-wuerttemberg")

# BAY

bayern_vote <-
  brd %>%
  html_nodes(xpath = '//*[@id="mw-content-text"]/div/table[6]') %>%
  html_table() %>%
  .[[1]] %>%
  mutate(land = "bayern")

bayern_seats <-
  brd %>%
  html_nodes(xpath = '//*[@id="mw-content-text"]/div/table[7]') %>%
  html_table() %>%
  .[[1]] %>%
  mutate(land = "bayern")
# fix years here


# BER

westberlin_v <-
  brd %>%
  html_nodes(xpath = '//*[@id="mw-content-text"]/div/table[10]') %>%
  html_table() %>%
  .[[1]] %>%
  mutate(land = "berlin")

fullberlin_v <-
  brd %>%
  html_nodes(xpath = '//*[@id="mw-content-text"]/div/table[12]') %>%
  html_table() %>%
  .[[1]] %>%
  mutate(land = "berlin")

berlin_vote <- bind_rows(westberlin_v,fullberlin_v)

westberlin_s <-
  brd %>%
  html_nodes(xpath = '//*[@id="mw-content-text"]/div/table[11]') %>%
  html_table() %>% .[[1]] %>%
  mutate(land = "berlin")

fullberlin_s <-
  brd %>%
  html_nodes(xpath = '//*[@id="mw-content-text"]/div/table[13]') %>%
  html_table() %>% .[[1]] %>%
  mutate(land = "berlin")

berlin_seats <- bind_rows(westberlin_s,fullberlin_s)


# BRA

brandenburg_vote <-
  brd %>%
  html_nodes(xpath = '//*[@id="mw-content-text"]/div/table[14]') %>%
  html_table() %>%
  .[[1]] %>%
  mutate(land = "brandenburg")

brandenburg_seats <-
  brd %>%
  html_nodes(xpath = '//*[@id="mw-content-text"]/div/table[15]') %>%
  html_table() %>% .[[1]] %>%
  mutate(land = "brandenburg")

# BRE

bremen_vote <-
  brd %>%
  html_nodes(xpath = '//*[@id="mw-content-text"]/div/table[16]') %>%
  html_table() %>% .[[1]] %>%
  mutate(land = "bremen")

bremen_s1 <-
  brd %>%
  html_nodes(xpath = '//*[@id="mw-content-text"]/div/table[17]') %>%
  html_table() %>% .[[1]]

bremen_s2 <-
  brd %>%
  html_nodes(xpath = '//*[@id="mw-content-text"]/div/table[18]') %>%
  html_table() %>% .[[1]]

bremen_seats <- left_join(bremen_s1,bremen_s2,by = "Jahr")  %>%
  mutate(land = "bremen")

# Hamburg

hamburg_vote <-
  brd %>%
  html_nodes(xpath = '//*[@id="mw-content-text"]/div/table[19]') %>%
  html_table() %>%
  .[[1]] %>%
  .[-nrow(.),]  %>%
  mutate(land = "hamburg")
hamburg_vote[hamburg_vote == "CDU"] <- NA

hamburg_seats <-
  brd %>%
  html_nodes(xpath = '//*[@id="mw-content-text"]/div/table[20]') %>%
  html_table() %>%
  .[[1]] %>%
  .[-nrow(.),] %>%
  mutate(land = "hamburg")
hamburg_seats[hamburg_seats == "CDU"] <- NA

# Hessen

hessen_vote <-
  brd %>%
  html_nodes(xpath = '//*[@id="mw-content-text"]/div/table[21]') %>%
  html_table() %>%
  .[[1]] %>%
  mutate(land = "hessen")
hessen_vote[hessen_vote == "FDP"] <- NA

hessen_seats <-
  brd %>%
  html_nodes(xpath = '//*[@id="mw-content-text"]/div/table[22]') %>%
  html_table() %>% .[[1]] %>%
  mutate(land = "hessen")
hessen_seats[hessen_seats == "FDP"] <- NA


# MeckPom

mecklenburgvorpommern_vote <-
  brd %>%
  html_nodes(xpath = '//*[@id="mw-content-text"]/div/table[23]') %>%
  html_table() %>% .[[1]] %>%
  mutate(land = "mecklenburg-vorpommern")

mecklenburgvorpommern_seats <-
  brd %>%
  html_nodes(xpath = '//*[@id="mw-content-text"]/div/table[24]') %>%
  html_table() %>% .[[1]] %>%
  mutate(land = "mecklenburg-vorpommern")

# niedersachsen

niedersachsen_vote <-
  brd %>%
  html_nodes(xpath = '//*[@id="mw-content-text"]/div/table[25]') %>%
  html_table() %>% .[[1]] %>%
  mutate(land = "niedersachsen")

niedersachsen_seats <-
  brd %>%
  html_nodes(xpath = '//*[@id="mw-content-text"]/div/table[26]') %>%
  html_table() %>% .[[1]] %>%
  mutate(land = "niedersachsen")

# NRW

nordrheinwestfalen_vote <-
  brd %>%
  html_nodes(xpath = '//*[@id="mw-content-text"]/div/table[27]') %>%
  html_table() %>% .[[1]] %>%
  mutate(land = "nordrhein-westfalen")

nordrheinwestfalen_seats <-
  brd %>%
  html_nodes(xpath = '//*[@id="mw-content-text"]/div/table[28]') %>%
  html_table() %>% .[[1]] %>%
  mutate(land = "nordrhein-westfalen")

# RLP

rheinlandpfalz_vote <-
  brd %>%
  html_nodes(xpath = '//*[@id="mw-content-text"]/div/table[29]') %>%
  html_table() %>% .[[1]] %>%
  mutate(land = "rheinland-pfalz")

rheinlandpfalz_seats <-
  brd %>%
  html_nodes(xpath = '//*[@id="mw-content-text"]/div/table[30]') %>%
  html_table() %>% .[[1]] %>%
  mutate(land = "rheinland-pfalz")

# Saarland

saarland_vote <-
  brd %>%
  html_nodes(xpath = '//*[@id="mw-content-text"]/div/table[31]') %>%
  html_table() %>% .[[1]] %>%
  mutate(land = "saarland")

saarland_seats <-
  brd %>%
  html_nodes(xpath = '//*[@id="mw-content-text"]/div/table[32]') %>%
  html_table() %>% .[[1]] %>%
  mutate(land = "saarland")


# Sachsen

sachsen_vote <-
  brd %>%
  html_nodes(xpath = '//*[@id="mw-content-text"]/div/table[33]') %>%
  html_table() %>% .[[1]] %>%
  mutate(land = "sachsen")

sachsen_seats <-
  brd %>%
  html_nodes(xpath = '//*[@id="mw-content-text"]/div/table[34]') %>%
  html_table() %>% .[[1]] %>%
  mutate(land = "sachsen")

# Sachsenanhalt

sachsenanhalt_vote <-
  brd %>%
  html_nodes(xpath = '//*[@id="mw-content-text"]/div/table[35]') %>%
  html_table() %>% .[[1]] %>%
  mutate(land = "sachsen-anhalt")

sachsenanhalt_seats <-
  brd %>%
  html_nodes(xpath = '//*[@id="mw-content-text"]/div/table[36]') %>%
  html_table() %>% .[[1]] %>%
  mutate(land = "sachsen-anhalt")


# Schleswig Holstein

schleswigholstein_vote <-
  brd %>%
  html_nodes(xpath = '//*[@id="mw-content-text"]/div/table[37]') %>%
  html_table() %>% .[[1]] %>%
  mutate(land = "schleswig-holstein")

schleswigholstein_seats <-
  brd %>%
  html_nodes(xpath = '//*[@id="mw-content-text"]/div/table[38]') %>%
  html_table() %>% .[[1]] %>%
  mutate(land = "schleswig-holstein")

# Thüringen

thueringen_vote <-
  brd %>%
  html_nodes(xpath = '//*[@id="mw-content-text"]/div/table[39]') %>%
  html_table() %>% .[[1]] %>%
  mutate(land = "thueringen")

thueringen_seats <-
  brd %>%
  html_nodes(xpath = '//*[@id="mw-content-text"]/div/table[40]') %>%
  html_table() %>% .[[1]] %>%
  mutate(land = "thueringen")



votevars <- ls() [ls() %>% str_detect("vote")]
seatvars <- ls() [ls() %>% str_detect("seats")]

fullvote <-
  mget(votevars) %>% bind_rows()

fullseats <-
  mget(seatvars) %>%
  map(.f = function(x) x %>% mutate_if(is.integer,as.character))%>%
  bind_rows()

# Management

as.commanumeric <- function(x) x %>% str_replace_all(",",".") %>% as.character() %>% as.numeric()


fullvote %<>%
  mutate_at(vars(Wahltag),
            funs(as.Date(.,format = "%d.%m.%Y"))) %>%
  mutate_at(vars(-Wahltag,-land),
            funs(as.commanumeric)) %>%
  mutate(FDP = rowSums(select(.,FDP,`FDP/DVP`,`FDP/DPS`),na.rm = T)) %>%
  select(-`FDP/DVP`,-`FDP/DPS`)

# This is coded with survivorship bias: only contemporarily important parties included.

main_parties <- c("CDU","SPD","CSU","FDP","Grüne","AfD","Linke") #"REP","NPD")

# export this?
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

fullvote %<>%
  mutate(others = 100 - rowSums(select(., one_of(main_parties)),na.rm = T)) %>%
  select(one_of(c("Wahltag","Wbt.","land",main_parties,"others"))) %>%
  rename(wbt = Wbt.,
         date = Wahltag) %>%
  mutate(year = format(date,"%Y"))

colnames(fullvote) %<>%
  purrr::map_chr(.f = remove_umlaut) %>%
  purrr::map_chr(.f = str_to_lower)

fullvote$year
names(fullvote)
names(bundestag_laender)


landtagswahlen <-
  fullvote %>%
  mutate(level = "landtagswahl") %>%
  mutate_at(vars(wbt,cdu:others),
            funs(./100))

devtools::use_data(landtagswahlen,overwrite = TRUE)







