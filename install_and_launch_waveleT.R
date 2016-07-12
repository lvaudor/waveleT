#depuis projet waveleT.proj
setwd("..")

## a faire tourner apres avoir depose scripts dans R
devtools::document("waveleT")

require(tools)
pdb <- available.packages()
deps<- package_dependencies(packages = c("shiny","wavelets","biwavelet"), pdb,
                            which = c("Imports","Depends"),
                            recursive = TRUE, reverse = FALSE)
deps=as.vector(unlist(deps))
deps=paste0(deps, collapse=",")


devtools::install("waveleT")
#devtools::use_vignette("waveleT_vignette",pkg="waveleT")

require(waveleT)
waveleT()
