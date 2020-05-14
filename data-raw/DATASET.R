## code to prepare `DATASET` dataset goes here

## Download fonts from https://github.com/google/fonts
# https://github.com/google/fonts/archive/master.zip

fonts <- list.files("data-raw/fonts-master", pattern = ".ttf",
                    recursive = TRUE, full.names = TRUE)

library(tidyverse)
popular <- read_csv("data-raw/popular-fonts.csv")[[1]]

selected_fonts <- lapply(popular, function(x){
  fonts[grepl(gsub(" ","",x), fonts)]
})

lapply(unlist(selected_fonts), function(font){
  file.copy(font, "inst/fonts")
})

usethis::use_data(DATASET, overwrite = TRUE)
