#' lala
#'
#' Bundestag election results for the regional laender level 1949-2017
#'
#' @format A data frame with 236 rows and 13 variables:
#' \describe{
#'   \item{year}{election year}
#'   \item{land}{regional level}
#'   \item{date}{election date}
#'   \item{...}{party vote share vector}
#'   \item{wbt}{voter turnout}
#'   \item{others}{other parties}
#'   \item{level}{Level of Election (federal or regional)}
#' }
"bundestag_laenderebene"

#' German "Landtagswahlen" election results for the regional laender level 1946-2018
#'
#' @format A data frame with 233 rows and 13 variables:
#' \describe{
#'   \item{year}{election year}
#'   \item{land}{regional level}
#'   \item{date}{election date}
#'   \item{...}{party vote share vector}
#'   \item{wbt}{voter turnout}
#'   \item{others}{other parties}
#'   \item{level}{Level of Election (federal or regional)}
#' }
"landtagswahlen"

#' German "Bundestagswahlen" election results for the constituency/district level (1949-2017)
#'
#' @format A data frame with 10633 rows and 74 variables:
#' \describe{
#'   \item{year}{election year}
#'   \item{WKID}{constituency ID}
#'   \item{type}{first or second vote}
#'   \item{WKN}{constituency name}
#'   \item{WKÜ}{state affiliation}
#'   \item{wahlberechtigte}{registered voters}
#'   \item{waehler}{voters}
#'   \item{ungueltige}{invalid votes}
#'   \item{gueltige}{valid votes}
#'   \item{...}{other parties}
#' }
"btw_wkebene"

#' German "Bundestagswahlen" election results for the constituency level 1949-2017
#'
#' @format A data frame with 10633 rows and 74 variables:
#' \describe{
#'   \item{year}{election year}
#'   \item{WKID}{constituency ID}
#'   \item{type}{first or second vote}
#'   \item{WKN}{constituency name}
#'   \item{WKÜ}{state affiliation}
#'   \item{wahlberechtigte}{registered voters}
#'   \item{waehler}{voters}
#'   \item{ungueltige}{invalid votes}
#'   \item{gueltige}{valid votes}
#'   \item{...}{other parties}
#' }
"btw_districtlevel"


#' Input Data for structural model
#'
#' @format A data frame with 1864 rows and 25 variables:
#' \describe{
#'   \item{year}{election year}
#'   \item{land}{regional level}
#'   \item{date}{election date}
#'   \item{wbt}{voter turnout}
#'   \item{party}{party name}
#'   \item{vote}{vote share}
#'   \item{partytype}{classification in spd/union/small and others}
#'   \item{lag_ltw}{one-period lag of landtagswahl result}
#'   \item{lag_btw}{one-period lag of bundestagswahl result (of that party in that state)}
#'   \item{date_btw}{date of the last bundestagswahl}
#'   \item{cabinet}{name of the cabinet}
#'   \item{primeminister_name}{name of the incumbent prime minister}
#'   \item{party_x}{1st to 3rd party listing coalition members (1st party is party of PM)}
#'   \item{start}{year that the incumbent government was formed}
#'   \item{end}{year it was replaced}
#'   \item{primeminister}{Logical T/F if party is holding incumbency of PM}
#'   \item{gov}{Logical T/F if party is incumbent coalition member}
#'   \item{juniorpartner}{T/F if party is junior partner (gov but not PM)}
#'   \item{bip}{absolute bip in billion €}
#'   \item{bipchange}{change in bip in the last 2 years prior to the election}
#'   \item{firsttime}{Logical T/F if party is firsttime contender}
#'   \item{distance_btw_lag}{distance to last Bundestagswahl in days}
#'   \item{others}{other parties}
#' }
"structural_modeldata"

#' German regional government cabinets for the whole post-war period
#'
#' @format A data frame with 325 rows and 11 variables:
#' \describe{
#'   \item{land}{regional level}
#'   \item{cabinet}{name of the respective cabinet}
#'   \item{years}{years cabinet was in office}
#'   \item{dates}{specific dates cabinet was in office}
#'   \item{parties}{parties forming the coalition}
#'   \item{primeminister}{name of the government leader}
#'   \item{party_x}{name of the forming parties}
#'   \item{start}{year in which gov't started}
#'   \item{end}{year in which it ended}
#' }
"landesregierungen"

#' German regional GDP data
#'
#' @format A data frame with 27 rows and 18 variables:
#' \describe{
#'   \item{year}{regional level}
#'   \item{...}{name of the bundesland}
#'   \item{d}{Germany total sum}
#' }
"laenderbip"



