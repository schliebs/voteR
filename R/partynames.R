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
    data.frame(koa = c("jamaika","schwarzgelb","rotgruen","groko","rotrotgruen","ampel","schwarzgruen"),
               v1 =  c("cdu","cdu","spd","cdu","spd","spd","cdu"),
               v2 =  c("fdp","fdp","gruene","spd","gruene","gruene","gruene"),
               v3 =  c("csu","csu","","csu","linke","fdp","csu"),
               v4 =  c("gruene","","","","","","")) %>%
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
  return(p)
}





