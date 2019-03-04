wahlrecht_urls_bund <- read.delim('data-raw/wahlrecht/list_urls_bund.txt',stringsAsFactors = F,header = F)$V1

usethis::use_data(wahlrecht_urls_bund,internal = F,overwrite = T)
