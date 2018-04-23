
# reproducible example
library(tidyverse)

set.seed(2)
df <-
  data.frame(
    x =  sample(seq(as.Date('2000/01/01'), as.Date('2015/01/01'), by="day"), 10),
    group = sample(c("A","B"),10,replace = T),
    value = sample(1:10,size=10)
  ) %>% arrange(x)

df <- df %>%
  group_by(group) %>%
  mutate(owngroup_lag = lag(value))


df %>% data.frame(othergroup_lag = c(NA,1,2,7,7,9,10,10,8,6))


## Ben
df_neu <- cbind.data.frame(df, c(rep("SPD", 5), rep("CDU", 5)))
names(df_neu)[5] <- "party"
lagA <- lagB <- c()
for(i in 1:length(df)){
  tempA <- df_neu$value[1:i][df_neu$party[1:i] == df_neu$party[i] & df_neu$group[1:i] == "A"][1]
  tempB <- df_neu$value[1:i][df_neu$party[1:i] == df_neu$party[i] & df_neu$group[1:i] == "B"][1]

  if(length(tempA)==0){tempA <- NA}
  if(length(tempB)==0){tempB <- NA}

  lagA <- c(lagA, tempA)
  lagB <- c(lagB, tempB)
}
