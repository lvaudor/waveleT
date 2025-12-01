library(waveleT)
library(tidyverse)
data <- read_csv2("~/Bureau/blog_ondelettes/toy_humeur.csv") %>%
  select(humeur) %>%
  as.matrix()
data(data_waveleT)
n=nrow(data_waveleT)
jmax=floor(log(n)/log(2))
mra_obj=wavelets::mra(data_waveleT,
                      n.levels = jmax,
                      method = "modwt",
                      boundary = "reflection",
                      filter = "la8")

MRAsum(mradata, signalname="y1", my_wt_filter="la8", levels)
