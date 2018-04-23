partynames <- list()

## France

france <- list()

france[["lrem"]] <- list(id = "lrem",
                         longlabel = "La République En Marche!",
                         shortlabel = "LREM",
                         selectors = c("lrem","en marche","la république en marche"))


## Germany

germany <- list()
germany[["cdu"]] <- list(id = "cdu",
                         longlabel = "Christlich-Demokratische Union",
                         shortlabel = "CDU",
                         selectors = c("cdu","Christliche-Demokratische Union","Christliche-Demokratische Union Deutschlands"))

germany[["csu"]] <- list(id = "csu",
                         longlabel = "Christlich-Soziale Union",
                         shortlabel = "CSU",
                         selectors = c("csu","Christliche-Soziale Union"),
                         color = "black")

germany[["spd"]] <- list(id = "spd",
                         longlabel = "Sozialdemokratische Partei Deutschlands",
                         shortlabel = "SPD",
                         selectors = c("spd","Sozialdemokratische Partei Deutschlands"),
                         color = "red")

germany[["gruene"]] <- list(id = "gruene",
                         longlabel = "Bündnis 90/Die Grünen",
                         shortlabel = "Grüne",
                         selectors = c("gruene","grüne","buendnis90"),
                         color = c("green"))

germany[["fdp"]] <- list(id = "gruene",
                         longlabel = "Freiheitlich-Demokratische Partei Deutschlands",
                         shortlabel = "FDP",
                         selectors = c("fdp","Freiheitlich-Demokratische Partei Deutschlands","FDP/DPS","FDP/DVP"),
                         color = c("yellow")
                        )

germany[["afd"]] <- list(id = "gruene",
                            longlabel = "Alternative für Deutschland",
                            shortlabel = "AfD",
                            selectors = c("afd","Alternative für Deutschland"),
                            color = c("blue"))

germany[["linke"]] <- list(id = "linke",
                         longlabel = "Die Linke",
                         shortlabel = "Linke",
                         selectors = c("linke","die linke","dielinke","pds"),
                         color = c("purple"))


others_ltw <- read.table("data/others.txt",encoding = "UTF-8")

germany[["other"]] <- list(id = "other",
                            longlabel = "Andere Parteien",
                            shortlabel = "Andere",
                            selectors = stringr::str_to_lower(others_ltw$V1),
                            color = c("grey"))

partynames[["germany"]] <- germany

names(germany)

#######
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

partyname_contains("germany","csu")



# Get parties that are member of a certrain coalition

#' Get coalition members
#' @description Get parties that are member of a certrain coalition
#' @param coalition Character string containing the name of the coalition. \cr
#' Options are c("jamaika","schwarzgelb","rotgruen","groko","rotrotgruen","ampel","schwarzgruen").
#' @return A vector containing all parties included in the coalition.
#' @examples
#' koa_members("schwarzgelb")
#' @export
koa_members <- function(koalition) {

  df_koa <-
    data.frame(koa = c("sozialliberal","jamaika","schwarzgelb","rotgruen","groko","rotrotgruen","ampel","schwarzgruen"),
               v1 =  c("spd","cdu","cdu","spd","cdu","spd","spd","cdu"),
               v2 =  c("fdp","fdp","fdp","gruene","spd","gruene","gruene","gruene"),
               v3 =  c("","csu","csu","","csu","linke","fdp","csu"),
               v4 =  c("","gruene","","","","","","")) %>%
    mutate_all(funs(as.character(.)))

  logical <- df_koa$koa == koalition

  partiesin <- df_koa[df_koa$koa == koalition,2:5]

  n <- length(which(partiesin != ""))

  a <- df_koa [logical,2]
  b <- df_koa [logical,3]
  c <- df_koa [logical,4]
  d <- df_koa [logical,5]

  members <- c(a,b,c,d)
  members <- members[stringr::str_length(members)>0]

  return (members)
}





# Get coalitions

#' Get coalitions
#' @description Get all available coalitions
#' @return The vector including all coalitions.
#' @examples
#' koas(year = 2017)
#' @export
koas <- function(year = 2017){
  if(year == 2017) koas <- c("schwarzgelb","rotgruen","groko","rotrotgruen","ampel","schwarzgruen","jamaika")
  if(year == 2013) koas <- c("schwarzgelb","rotgruen","groko","rotrotgruen","ampel","schwarzgruen")
  if(year == 2009) koas <- c("schwarzgelb","rotgruen","groko","rotrotgruen","ampel","schwarzgruen","jamaika","sozialliberal")
  return(koas)
}

# Get main parties for Gles analysis

#' Get parties
#' @description Get main parties for Gles analysis
#' @return The vector including all parties
#' @examples
#' parties(year = 2017)
#' @export
parties <- function(year = 2017){
  if(year == 2017) p <- c("cdu","csu","spd","gruene","fdp","linke","afd")
  if(year == 2013) p <- c("cdu","csu","spd","gruene","fdp","linke")
  if(year == 2009) p <- c("cdu","csu","spd","gruene","fdp","linke")
  return(p)
}


# Länder
laender <-
  data.frame(id = c("baden-wuettemberg",
                    "bayern",
                    "berlin",
                    "brandenburg",
                    "bremen",
                    "hamburg",
                    "hessen",
                    "mecklenburg-vorpommern",
                    "niedersachsen",
                    "nordrhein-westfalen",
                    "rheinland-pfalz",
                    "saarland",
                    "sachsen",
                    "sachsen-anhalt",
                    "schleswig-holstein",
                    "thueringen"),
          #   nrnew = c(),
               fullabel = c("Baden-Württemberg",
                            "Bayern",
                            "Berlin",
                            "Brandenburg",
                            "Bremen",
                            "Hamburg",
                            "Hessen",
                            "Mecklenburg-Vorpommern",
                            "Niedersachsen",
                            "Nordrhein-Westfalen",
                            "Rheinland-Pfalz",
                            "Saarland",
                            "Sachsen",
                            "Sachsen-Anhalt",
                            "Schleswig-Holstein",
                            "Thüringen"),
                         shortlabel = c("BW",
                                 "BY",
                                 "BE",
                                 "BB",
                                 "HB",
                                 "HH",
                                 "HE",
                                 "MV",
                                 "NI",
                                 "NW",
                                 "RP",
                                 "SL",
                                 "SN",
                                 "ST",
                                 "SH",
                                 "TH"))

devtools::use_data(laender,overwrite = TRUE)






