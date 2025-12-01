data_waveleT=read.csv("data-raw/data_example.csv", header=TRUE,dec=".", sep=";")
usethis::use_data(data_waveleT, overwrite = TRUE)

library(tidyverse)
cat_moods=readr::read_csv("data-raw/humeur_chat.csv") %>%
  dplyr::mutate(x=as.POSIXct(x, format="%Y-%m-%d %H:%M:%S", tz="UTC"))
usethis::use_data(cat_moods,overwrite=TRUE)
