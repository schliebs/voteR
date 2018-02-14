#!!!! https://tex.stackexchange.com/questions/125274/error-font-ts1-zi4r-at-540-not-found !!!

pack <- "runR"
 path <- "C:/Users/Schliebs/OneDrive/github/runR" #getwd()#find.package(pack)#

 system(paste(shQuote(file.path(R.home("bin"), "R")),
               "CMD", "Rd2pdf", shQuote(path)))
