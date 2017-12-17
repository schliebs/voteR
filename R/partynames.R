partynames <- list()

## France

france <- list()
france[["em"]] <- c("en marche","em")
france[["lr"]] <- c("les republicains","lr")
france[["ps"]] <- c("party socialiste","ps")


## Germany

germany <- list()
germany[["cdu"]] <- c("cdu","christlich demokratische union")
germany[["csu"]] <- c("csu","christlich soziale union")
germany[["union"]] <- c("union")

#
partynames[["germany"]] <- germany
partynames[["france"]] <- france

country <- "germany"
token <- "christlich"

partyname_contains <- function(country = "germany",
                               token = "christlich"){
  list = partynames[[country]]
  unlist(
    lapply(list,
         function(x) {
                      which(stringr::str_detect (x,
                                           stringr::str_to_lower(c(token))))
                      }
         )
  ) %>% names()
}

partyname_contains("france","EM")

