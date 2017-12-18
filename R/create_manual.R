 pack <- "voteR"
 path <- "C:/Users/Schliebs/OneDrive/github/voteR" #getwd()#find.package(pack)#

 system(paste(shQuote(file.path(R.home("bin"), "R")),
               "CMD", "Rd2pdf", shQuote(path)))
