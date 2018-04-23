btw <- read.csv2(file = "data/bundestag_all.csv") %>%
  mutate_at(vars(X,WKID,year),as.character()) %>%
  mutate_if(is.factor,as.character)

head(btw)

str(btw)

btw2 <-
  btw %>%

  group_by(year,votes,land) %>%
  summarise_if(is.numeric,sum)

btw$land %>% table(useNA = "always")
