####################################################################################################
####################################################################################################
###### Preparation of harvesting plan
######## This is sourced at the beginning of the experiment 
# 
# uaf_RAT <- read.csv("uaf_RAT.csv")
# subZones_RAT <- read.csv("subZones_RAT.csv")
# coverTypes_RAT <- read.csv("coverTypes_RAT.csv")

uafHarvest <- uaf_RAT$ID # UAF IDs of all elligible UAFs
subZoneHarvest <- subZones_RAT[which(subZones_RAT$value == "Productive forest - harvestable"), "ID"]  # character vector of elligible subzones
# prodSpp <- c("EN", "PG", "F", "R") ## for age structure targets, may differ  deciduous
# prodSppId <- coverTypes_RAT[which(coverTypes_RAT$value %in% prodSpp),"ID"]
comSpp <- list(SEPM = c("EN", "PG", "R"))
comSppId <- lapply(comSpp, function(x) coverTypes_RAT[which(coverTypes_RAT$value %in% x),"ID"])
spMaturity <- c(EN = 90, PG = 76, F = NA, R = 90) # commercial maturity of each species
spMaturity <- spMaturity[coverTypes_RAT[match(names(spMaturity), names(spMaturity)), "ID"]]
names(spMaturity) <- 1:length(spMaturity)
## zonal grouping for age structure targets
targetAgeGrouping <- c("uaf", "subZones") # factors
targetHarvestLevels <- list(SEPM = c(0.0062))
regenMaxProp <- .35 # max proportion of regenerating stands
regenMaxAge <- 20 #less than x years since last disturbance
oldMinProp <- .14 # min proportion of old stands
oldMinAge <- 100


managementPlan <- list(baseline = list(uaf = uafHarvest,
                                       subZone = subZoneHarvest,
                                       # prodSppID = prodSppId,
                                       comSppId = comSppId,
                                       maturity = spMaturity,
                                       targetAgeGrouping = targetAgeGrouping,
                                       targetHarvestLevels = targetHarvestLevels,
                                       regenMaxProp =regenMaxProp,
                                       regenMaxAge = regenMaxAge,
                                       oldMinProp = oldMinProp,
                                       oldMinAge = oldMinAge)
                       )

rm(uafHarvest, subZoneHarvest, spMaturity,
   targetAgeGrouping, targetHarvestLevels, regenMaxProp, regenMaxAge,
   oldMinProp, oldMinAge)



