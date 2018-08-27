###################################################################################################
###################################################################################################
##### Compiling fire and harvest outputs to tidy data frames
##### Dominic Cyr, in collaboration with Tadeusz Splawinski, Sylvie Gauthier, and Jesus Pascual Puigdevall
rm(list = ls())
setwd("D:/regenFailureRiskAssessmentData_phase2/2018-08-24")
####################################################################################################
####################################################################################################
wwd <- paste(getwd(), Sys.Date(), sep = "/")
dir.create(wwd)
setwd(wwd)
#################
#require(rgdal)
require(raster)
#require(rgeos)
require(dplyr)
####################################################################

# studyArea <- raster("../studyArea.tif")
# fireZones <- raster("../fireZones.tif")
# fireZones_RAT <- read.csv("../fireZones_RAT.csv")
# 
# ## focusing on fireZones in studyArea
# z <- which(zonal(studyArea, fireZones, sum)[,"value"]>1)
# fireZones_RAT <- fireZones_RAT[z, ]
# fireZones[is.na(studyArea)] <- NA
# 
# 
# ##
# convFactor <- prod(res(studyArea))/10000### to convert to hectares
# ## in all simulation area
# fireZoneArea <- as.data.frame(zonal(!is.na(fireZones), fireZones, sum))
# fireZoneArea$value <- fireZoneArea$value*convFactor
# colnames(fireZoneArea) <- c("ID", "areaZoneTotal_ha")
# fireZoneArea <- merge(fireZoneArea, fireZones_RAT)
# # ## within study area
# # fireZoneAreaStudyArea <- as.data.frame(zonal(studyArea, fireZones, sum))
# # fireZoneAreaStudyArea$value <- fireZoneAreaStudyArea$value*convFactor
# # colnames(fireZoneAreaStudyArea) <- c("ID", "areaZoneStudyArea_ha")
# # ##
# # fireZoneArea <- merge(fireZoneArea,fireZoneAreaStudyArea)
# 
# # fireZoneArea[,"Fire_Cycle_studyArea"] <- fireZoneArea$Fire_Cycle
# # fireZoneArea[fireZoneArea$areaZoneStudyArea_ha== 0,"Fire_Cycle_studyArea"] <- NA
# 
# ### computing weight averaged fire cycles
# x <- fireZoneArea %>%
#     mutate(propTotalArea = areaZoneTotal_ha/sum(areaZoneTotal_ha),
#            #propStudyArea = areaZoneStudyArea_ha/sum(areaZoneStudyArea_ha),
#            pAABTotal = (1/Fire_Cycle) * propTotalArea)
# #pAABStudyArea = (1/Fire_Cycle) * propStudyArea)
# 
# fireZoneArea <- rbind(fireZoneArea,
#                       data.frame(ID = NA,
#                                  areaZoneTotal_ha = sum(fireZoneArea$areaZoneTotal_ha),
#                                  #areaZoneStudyArea_ha = sum(fireZoneArea$areaZoneStudyArea_ha),
#                                  Zone_LN = "total",
#                                  Fire_Cycle = round(1/sum(x$pAABTotal))#,
#                                  #Fire_Cycle_studyArea = round(1/sum(x$pAABStudyArea))
#                       )
# )
# 
# 
# 
# 
# ####################################################################
# ####################################################################
# ######
# ######      compiling fire simulation outputs
# ######
# outputFolder <- "../output"
# x <- list.files(outputFolder)
# index <- grep(".RData", x)
# index <- intersect(index, grep("Fire", x))
# x <- x[index]
# simInfo <- gsub(".RData", "", x)
# simInfo <- strsplit(simInfo, "_")
# #scenario <- as.character(lapply(simInfo, function(x) x[[2]]))
# replicates <- as.numeric(lapply(simInfo, function(x) x[2]))
# ###########################################
# ###########################################
# 
# require(doSNOW)
# require(parallel)
# require(foreach)
# # clusterN <- 2
# clusterN <-  25#max(1, floor(0.9*detectCores()))  ### choose number of nodes to add to cluster.
# #######
# cl = makeCluster(clusterN, outfile = "") ##
# registerDoSNOW(cl)
# #######
# outputCompiled <- foreach(i = seq_along(x), .combine = "rbind") %dopar% {# 
#     require(raster)
#     require(reshape2)
#     require(dplyr)
#     
#     ## simInfo
#     s <- "test"
#     # s <- scenario[i]
#     r <- replicates[i]
#     
#     ## fetching outputs
#     fire <- get(load(paste(outputFolder, x[i], sep="/")))
#     ## focusing on studyArea
#     fire[is.na(studyArea)] <- NA
#     
#     ## compiling realized area burned
#     
#     areaBurned <- t(zonal(fire, fireZones,  "sum")[,-1]) * convFactor
#     colnames(areaBurned) <- fireZones_RAT$Zone_LN
#     areaBurned <- data.frame(areaBurned, total = apply(areaBurned, 1, "sum"))
#     year <- as.numeric(gsub("[^0-9]", "", rownames(areaBurned)))
#     
#     
#     ## tidying up data frame
#     areaBurned <- data.frame(year, replicate = r, areaBurned)
#     
#     out <- melt(areaBurned,
#                 id.vars = c("year", "replicate"),
#                 variable.name = "Zone_LN",
#                 value.name = "areaBurnedTotal_ha")
#     
#     out <- data.frame(scenario = s, out)
#     
#     print(paste("fire", s, r))
#     return(out)
#     
# }
# 
# stopCluster(cl)
# 
# outputCompiled <-merge(outputCompiled, fireZoneArea)
# outputCompiled <- arrange(outputCompiled, scenario, replicate, year, Zone_LN)
# 
# save(outputCompiled, file = "outputCompiledFire.RData")
# rm(outputCompiled)
# 
# studyArea <- raster("../studyArea.tif")
# uaf <- raster("../uaf.tif")
# #subZones <- raster
# uaf_RAT <- read.csv("../uaf_RAT.csv")
# ##
# convFactor <- prod(res(studyArea))/10000### to convert to hectares
# 
# 
# ####################################################################
# ####################################################################
# ######
# ######      compiling harvest simulation outputs
# ######
# outputFolder <- "../output"
# x <- list.files(outputFolder)
# index <- grep(".RData", x)
# index <- intersect(index, grep("Harvest", x))
# x <- x[index]
# simInfo <- gsub(".RData", "", x)
# simInfo <- strsplit(simInfo, "_")
# #scenario <- as.character(lapply(simInfo, function(x) x[[2]]))
# replicates <- as.numeric(lapply(simInfo, function(x) x[2]))
# ###########################################
# ###########################################
# 
# require(doSNOW)
# require(parallel)
# require(foreach)
# # clusterN <- 2
# clusterN <-  25#max(1, floor(0.9*detectCores()))  ### choose number of nodes to add to cluster.
# #######
# cl = makeCluster(clusterN, outfile = "") ##
# registerDoSNOW(cl)
# #######
# outputCompiled <- foreach(i = seq_along(x), .combine = "rbind") %dopar% {# seq_along(x)
#     require(raster)
#     require(reshape2)
#     require(dplyr)
#     
#     ## simInfo
#     s <- "test"
#     # s <- scenario[i]
#     r <- replicates[i]
#     
#     ## fetching outputs
#     harv <- get(load(paste(outputFolder, x[i], sep="/")))
#     
#     ## compiling realized area burned
#     
#     areaHarvested <- t(zonal(harv, uaf,  "sum")[,-1]) * convFactor
#     areaHarvested <- data.frame(areaHarvested, total = apply(areaHarvested, 1, "sum"))
#     year <- as.numeric(gsub("[^0-9]", "", rownames(areaHarvested)))
#     colnames(areaHarvested)[uaf_RAT$ID] <- as.character(uaf_RAT[uaf_RAT$ID, "value"])
#     
#     ## keeping only zones intercepting study areazones with non-zero area harvested
#     z <- apply(areaHarvested, 2, sum)
#     areaHarvested <- areaHarvested[z>0]
#     
#     
#     ## tidying up data frame
#     areaHarvested <- data.frame(year, replicate = r, areaHarvested)
#     
#     out <- melt(areaHarvested,
#                 id.vars = c("year", "replicate"),
#                 variable.name = "uaf",
#                 value.name = "areaHarvestedTotal_ha")
#     
#     out$uaf <- gsub("X", "", out$uaf)
#     out$uaf <- gsub("\\.", "-", out$uaf)
#     out <- data.frame(scenario = s, out)
#     
#     print(paste("harvest", s, r))
#     return(out)
#     
# }
# 
# stopCluster(cl)
# outputCompiled <- arrange(outputCompiled, scenario, uaf, year)
# 
# save(outputCompiled, file = "outputCompiledHarvest.RData")
# 
# 
# 
# 
# ####################################################################
# ####################################################################
# ######
# ######      compiling age structure outputs
# ######
# studyArea <- raster("../studyArea.tif")
# uaf <- raster("../uaf.tif")
# uaf_RAT <- read.csv("../uaf_RAT.csv")
# subZones <- raster("../subZones.tif")
# subZones_RAT <- read.csv("../subZones_RAT.csv")
# coverTypes <- raster("../coverTypes.tif")
# ##
# convFactor <- prod(res(studyArea))/10000### to convert to hectares
# ###################################################################
# ## loading management plan (to fetch age structure targets, and productive cover types )
# managementPlan <- get(load("../managementPlan.RData"))
# plan <- managementPlan$baseline
# ## eligible to harvest
# harvEligible <- uaf %in% plan$uaf &
#     subZones %in% plan$subZone
# harvEligible[!harvEligible] <- NA
# 
# ## spp eligible (commercial species)
# spEligible <- coverTypes %in% plan$comSppId[["SEPM"]] & harvEligible
# spEligible[!spEligible] <- NA
# 
# ##
# # regenMaxProp <- plan$regenMaxProp
# regenMaxAge <- plan$regenMaxAge
# # oldMinProp <- plan$oldMinProp
# oldMinAge <- plan$oldMinAge
# 
# # ## targets
# eligibleArea <- t(zonal(spEligible, uaf,  "sum")[,-1]) * convFactor
# eligibleArea <- data.frame(uaf = as.character(uaf_RAT[uaf_RAT$ID, "value"]), managedAreaTotal_ha = as.numeric(eligibleArea))
# eligibleArea <- rbind(eligibleArea,
#                       data.frame(uaf = "total",
#                                  managedAreaTotal_ha = sum(eligibleArea$managedAreaTotal_ha)))
# 
# ####################################################################
# ####################################################################
# outputFolder <- "../output"
# x <- list.files(outputFolder)
# index <- grep(".RData", x)
# index <- intersect(index, grep("TSD", x))
# x <- x[index]
# simInfo <- gsub(".RData", "", x)
# simInfo <- strsplit(simInfo, "_")
# #scenario <- as.character(lapply(simInfo, function(x) x[[2]]))
# replicates <- as.numeric(lapply(simInfo, function(x) x[2]))
# ###########################################
# ###########################################
# 
# require(doSNOW)
# require(parallel)
# require(foreach)
# # clusterN <- 2
# clusterN <-  15#max(1, floor(0.9*detectCores()))  ### choose number of nodes to add to cluster.
# #######
# cl = makeCluster(clusterN, outfile = "") ##
# registerDoSNOW(cl)
# #######
# outputCompiled <- foreach(i = seq_along(x), .combine = "rbind") %dopar% {# seq_along(x)
#     require(raster)
#     require(reshape2)
#     require(dplyr)
#     
#     ## simInfo
#     s <- "test"
#     # s <- scenario[i]
#     r <- replicates[i]
#     
#     
#     ## fetching outputs
#     age <- get(load(paste(outputFolder, x[i], sep="/")))
#     ## focusing of forest eligible to harvest
#     age[is.na(spEligible)] <- NA
#     
#     ## compiling age structure
#     oldArea <- data.frame(t(zonal(age>=oldMinAge, uaf,  "sum")[,-1]) * convFactor)
#     regenArea <-  data.frame(t(zonal(age<regenMaxAge, uaf,  "sum")[,-1]) * convFactor)
#     
#     
#     year <- as.numeric(gsub("[^0-9]", "", rownames(oldArea)))
#     colnames(oldArea)[uaf_RAT$ID] <-
#         colnames(regenArea)[uaf_RAT$ID] <- as.character(uaf_RAT[uaf_RAT$ID, "value"])
#     
#     
#     ## total
#     oldArea[, "total"] <- apply(oldArea, 1, sum)
#     regenArea[, "total"] <- apply(regenArea, 1, sum)
#     
#     ## tidying up data frame
#     oldArea <- data.frame(year, replicate = r, oldArea)
#     regenArea <- data.frame(year, replicate = r, regenArea)
#     
#     
#     oldArea <- melt(oldArea,
#                     id.vars = c("year", "replicate"),
#                     variable.name = "uaf",
#                     value.name = "oldArea_ha")
#     
#     regenArea <- melt(regenArea,
#                       id.vars = c("year", "replicate"),
#                       variable.name = "uaf",
#                       value.name = "regenArea_ha")
#     
#     oldArea$uaf <- gsub("X", "", oldArea$uaf)
#     oldArea$uaf <- gsub("\\.", "-", oldArea$uaf)
#     oldArea <- data.frame(scenario = s,  oldArea)
#     
#     regenArea$uaf <- gsub("X", "", regenArea$uaf)
#     regenArea$uaf <- gsub("\\.", "-", regenArea$uaf)
#     regenArea <- data.frame(scenario = s,  regenArea)
#     
#     
#     summary(regenArea)
#     summary(oldArea)
#     out <- merge(regenArea, oldArea)
#     out <- merge(out, eligibleArea)
#     
#     print(paste(s, r))
#     return(out)
#     
# }
# 
# stopCluster(cl)
# outputCompiled <- arrange(outputCompiled, scenario, uaf, year, replicate)
# 
# save(outputCompiled, file = "outputCompiledAge.RData")




###################################################################################################
###################################################################################################
##### Compiling density outputs to tidy data frames
##### Dominic Cyr, in collaboration with Tadeusz Splawinski, Sylvie Gauthier, and Jesus Pascual Puigdevall
####################################################################

studyArea <- raster("../studyArea.tif")
coverTypes <- raster("../coverTypes.tif")
coverTypes_RAT <- read.csv("../coverTypes_RAT.csv")
densInit <- raster("../dens.tif")
dens_RAT <- read.csv("../dens_RAT.csv")
uaf <- raster("../uaf.tif")
subZones <- raster("../subZones.tif")
subZones_RAT <- read.csv("../subZones_RAT.csv")
###################################################################
## loading management plan (to fetch commercial cover types )
managementPlan <- get(load("../managementPlan.RData"))
plan <- managementPlan$baseline
## eligible to harvest

## spp eligible (commercial species)
spEligibleVals <- plan$comSppId[["SEPM"]]
spEligible <- coverTypes %in% spEligibleVals
spEligible[!spEligible] <- NA


## focusing on "EN" and "PG" covertypes
ctVal <- coverTypes
ctVal[!(coverTypes %in% which(coverTypes_RAT$ID %in% spEligibleVals))] <- NA
ctVal <- values(ctVal)
##
szVal <- subZones
szVal[szVal == subZones_RAT[which(subZones_RAT$value == "Non-forest"),"ID"]] <- NA
szVal <- values(szVal)

##
convFactor <- prod(res(studyArea))/10000### to convert to hectares
###################################################################
## loading management plan (to fetch age structure targets, and productive cover types )

# managementPlan <- get(load("../managementPlan.RData"))
# plan <- managementPlan$baseline
# ## eligible to harvest
# harvEligible <- uaf %in% plan$uaf &
#     subZones %in% plan$subZone
# harvEligible[!harvEligible] <- NA
# 
# ## spp eligible (commercial species)
# spEligible <- coverTypes %in% plan$comSppId[["SEPM"]] & harvEligible
# spEligible[!spEligible] <- NA
# 
# ##
# # regenMaxProp <- plan$regenMaxProp
# regenMaxAge <- plan$regenMaxAge
# # oldMinProp <- plan$oldMinProp
# oldMinAge <- plan$oldMinAge
# 
# # ## targets
# eligibleArea <- t(zonal(spEligible, uaf,  "sum")[,-1]) * convFactor
# eligibleArea <- data.frame(uaf = as.character(uaf_RAT[uaf_RAT$ID, "value"]), managedAreaTotal_ha = as.numeric(eligibleArea))
# eligibleArea <- rbind(eligibleArea,
#                       data.frame(uaf = "total",
#                                  managedAreaTotal_ha = sum(eligibleArea$managedAreaTotal_ha)))

####################################################################
####################################################################
outputFolder <- "../output"
x <- list.files(outputFolder)
index <- grep(".RData", x)
index <- intersect(index, grep("Density", x))
x <- x[index]
simInfo <- gsub(".RData", "", x)
simInfo <- strsplit(simInfo, "_")
#scenario <- as.character(lapply(simInfo, function(x) x[[2]]))
replicates <- as.numeric(lapply(simInfo, function(x) x[2]))
###########################################
###########################################

require(doSNOW)
require(parallel)
require(foreach)
# clusterN <- 2
clusterN <-  15#max(1, floor(0.9*detectCores()))  ### choose number of nodes to add to cluster.
#######
cl = makeCluster(clusterN, outfile = "") ##
registerDoSNOW(cl)
#######
outputCompiled <- foreach(i = seq_along(x), .combine = "rbind") %dopar% {# 
    require(raster)
    require(reshape2)
    require(dplyr)
    
    ## simInfo
    s <- "test"
    # s <- scenario[i]
    r <- replicates[i]
    
    
    ## fetching outputs
    density <- get(load(paste(outputFolder, x[i], sep="/")))
    ## focusing on 
    density[is.na(spEligible)] <- NA
    ## compiling density structure by species and subzones
    out <- as.data.frame(values(density))
    vals <- colnames(out)    
    out <- cbind(ctVal, szVal, out)
    out <- melt(out, measure.vars = vals)
    out <- out[complete.cases(out),]
    out <- out %>%
        group_by(ctVal, szVal, variable, value) %>%
        summarise(densClassArea_ha = n()*convFactor)
    
    out <- data.frame(scenario = s, 
                      replicate = r,
                      coverTypes = coverTypes_RAT[match(out$ctVal, coverTypes_RAT$ID), "value"],
                      subZone = subZones_RAT[match(out$szVal, subZones_RAT$ID), "value"],
                      year = gsub("CL_DENS.", "", out$variable),
                      dens = dens_RAT[match(out$value, dens_RAT$ID), "value"],
                      area_ha = out$densClassArea_ha)
    
    print(paste(s, r))
    return(out)
    
}

stopCluster(cl)
#outputCompiled <- arrange(outputCompiled, scenario, uaf, year, replicate)

save(outputCompiled, file = "outputCompiledDensity.RData")




