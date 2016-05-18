#depuis projet waveleT.proj
setwd("..")

## a faire tourner apres avoir depose scripts dans R
devtools::document("waveleT")
#devtools::use_vignette("waveleT_vignette",pkg="waveleT")
devtools::install("waveleT")

require(waveleT)
waveleT()
