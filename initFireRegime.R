####################################################################################################
####################################################################################################
###### Preparation of fire regime

### logNormal fit for fire size distribution
require(MASS)
fireObs <- read.csv("../data/fireObs.csv", header=TRUE)

maxFireSize <- 1.5*max(fireObs$areaTotal_ha) ## 1.5 * max obs => average fc of 104 years in the study area 

fireSizeFit <- fireSizeMax <- list()
for (i in unique(as.character(fireObs$zone))) {
    x <- filter(fireObs, zone == i)$areaTotal_ha
    fireSizeFit[[i]] <- fitdistr(x, "lognormal")
    fireSizeMax[[i]] <- maxFireSize
}

source("../scripts/simFireFnc.R")


