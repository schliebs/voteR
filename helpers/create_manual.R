#!!!! https://tex.stackexchange.com/questions/125274/error-font-ts1-zi4r-at-540-not-found !!!
'
devtools::document()
devtools::build()

############

pack <- "voteR"
path <- "C:/Users/MS/OneDrive - Zeppelin-University gGmbH/github/packages/voteR" #getwd()#find.package(pack)#

file.remove("C:/Users/MS/OneDrive - Zeppelin-University gGmbH/github/packages/voteR/voteR.pdf")
system(paste(shQuote(file.path(R.home("bin"), "R")),"CMD", "Rd2pdf", shQuote(path)))

source("helpers/update_yaml.R")
update_yaml("voteR", overwrite = T)

pkgdown::build_site()

'


