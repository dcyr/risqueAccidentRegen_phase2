###################################################################################################
###################################################################################################
##### Main script driving the simulation
##### Dominic Cyr, in collaboration with Tadeusz Splawinski, Sylvie Gauthier, and Jesus Pascual Puigdevall
rm(list = ls())
home <- path.expand("~")
home <- gsub("/Documents", "", home) # necessary on my Windows machine
setwd(paste(home, "Sync/Travail/ECCC/regenFailureRiskAssessment_phase2", sep ="/"))
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
########## RUN FIRST - RUN FIRST - RUN FIRST - RUN FIRST ##########
###Preparing initial conditions, 
########## RUN FIRST - RUN FIRST - RUN FIRST - RUN FIRST ##########
print(paste0("Preparing INITIAL CONDITIONS: ", normalizePath("../"), "initRasterPrep.R"))
source("../initRasterPrep.R")
tsdInit <- tsd
densInit <- dens
####################################################################################################
####################################################################################################

# #######
nRep <- 1000
simDuration <- 50
simStartYear <- 2015


## Reading management scenarios
print(paste0("Preparing MANAGEMENT SCENARIOS from source file: ", normalizePath("../"), "initHarvest.R"))
source("../initHarvest.R")
save(managementPlan, file = "managementPlan.RData")

## Loading fire regime(s)
fireRegime <- read.csv("../data/fireRegime_baseline.csv")
write.csv(fireRegime, file = "fireRegime_baseline.csv", row.names = F)
## Sourcing fire engine
source("../initFireRegime.R")

## Loading regeneration module
source("../scripts/regenDensityPredictFnc.R")



### actual simulation
require(doSNOW)
#require(parallel)
clusterN <-  40#max(1, floor(0.98*detectCores()))  ### choose number of nodes to add to cluster.
# #######

outputDir <-  paste(getwd(), "output/", sep = "/")
dir.create(outputDir)

cl = makeCluster(clusterN, outfile = "") ##
registerDoSNOW(cl)

foreach(i = 0:(nRep-1),
        .packages= names(sessionInfo()$otherPkgs)) %dopar%  {

    t1 <- Sys.time()
    require(stringr)
    simID <- str_pad(i, nchar(nRep-1), pad = "0")
    print("##############################################################")
    print("##############################################################")
    print(paste("simulating replicate", i))
    print("##############################################################")
    print("##############################################################")
    
    harvestScenario <- "baseline"
    tsd <- tsdInit
    dens <- densInit
    
    ### preparing management plan inputs
    plan <- managementPlan[[harvestScenario]]
    
    ## eligible to harvest
    harvEligible <- uaf %in% plan$uaf &
        subZones %in% plan$subZone
    harvEligible[!harvEligible] <- NA
    
    ## spp eligible (commercial species)
    spEligible <- coverTypes %in% plan$comSppId[["SEPM"]] & harvEligible
    spEligible[!spEligible] <- NA
    
    # ## all productive stands (may differ from commercial spp)
    # prodSpp <- coverTypes %in% plan$prodSppID & harvEligible
    # prodSpp[!prodSpp] <- NA
    
    ## commercial maturity thresholds
    matThresh <- coverTypes
    matThresh[is.na(spEligible)] <- NA
    matThresh[] <- plan$maturity[values(matThresh)]
      
    
    ## creating raster stacks by UAFs
    uR <- uSp <- uThresh <- uProd <- list()
    for (u in plan$uaf) {
        ## eligible to harvest within uaf
        uR[[u]] <- uaf==u & coverTypes
        # ## all productive stands within uaf (for computing age structure targets)
        # uProd[[u]] <- uaf==u & prodSpp
        ## all eligible species within uaf
        uSp[[u]] <- spEligible & uR[[u]]
        ## maturity thresholds within uaf
        uThresh[[u]] <- matThresh
        uThresh[[u]][!uR[[u]] & !uSp[[u]]] <- NA
        
    }
    ## putting all this in nice stacks
    uR <- stack(uR)
    spEligible <- stack(uSp)
    matThresh <- stack(uThresh)
    #uProd <- stack(uProd)
    rm(uSp, uThresh)
    ## uniformizing names
    names(uR) <- names(spEligible) <- names(matThresh) <- plan$uaf
    
    
    fire <- harv <- age <- density <- list()
    
    for (y in 1:simDuration) {### change into foreach, and return 'fire' 'harv' and 'age' as a list, then reformat
        
        ####################### simulating fire
        
        f <- simFire(tsfInit = tsd, simDur = 1, yearInit = simStartYear + y,
                     fireZones = fireZones,
                     fireRegime = fireRegime,
                     fireSizeFit = fireSizeFit,
                     fireSizeMax = fireSizeMax,
                     id = simID)

        f <- f$tsf == 0
        f[!f] <- NA
        fire[[y]] <- f
        

        
        ####################### simulating regeneration density
        
        # focusing on burned cells that are located in study are and 
        index <- which(values(f & studyArea &
                                  dens %in% dens_RAT[which(dens_RAT$value %in% densProd), "ID"]))
        
        if(length(index)>0) {
            a <- dens[index]
            b <- tsd[index]
            c <- coverTypes[index]
            
            newDens <- regenDensityPredict(dens = a,
                                           tsd = b,
                                           coverTypes = c,
                                           dens_RAT = dens_RAT,
                                           coverTypes_RAT = coverTypes_RAT)
            
            dens[index] <- newDens 
        }
        
        density[[y]] <- dens
        
        
        ####################### updating tsd (do after updating density)
        tsd[f] <- 0
        
        
        ####################### simulating harvest
        print(paste0("simulating harvests ; sim ", simID, " ; year ", y))

        h <- f
        rm(f)
        h[] <- NA
        ## eligible to harvest at a given timestep
        #####
        eligible <- tsd > matThresh &
            dens %in% dens_RAT[which(dens_RAT$value %in% densProd), "ID"]  ## 
        
        x <- numeric() ## vector of cells to be harvested

        for (u in plan$uaf) {
            ## checking for age structure conditions within uaf
            old <- tsd>=plan$oldMinAge & !is.na(coverTypes) & spEligible[[u]]
            regen <- tsd<plan$regenMaxAge & !is.na(coverTypes) & spEligible[[u]]

            ## computing proportions of old and regen
            propOld <- sum(values(old), na.rm = T) / sum(values(spEligible[[u]]), na.rm = T)
            propRegen <- sum(values(regen), na.rm = T) / sum(values(spEligible[[u]]), na.rm = T)
            ## computing remaining
            marginOld <- propOld - plan$oldMinProp
            marginRegen <- plan$regenMaxProp - propRegen

            if(marginOld > 0  &
               marginRegen > 0) {
                ## determining the number of cells to harvest
                p <- min(plan$targetHarvestLevels[["SEPM"]], marginOld, marginRegen)
                nCell <- round(p * sum(values(spEligible[[u]]), na.rm = T))
                ## eligible cells
                index <- which(values(eligible[[u]]))
                ## sampling cells
                x <- append(x, sample(index, size = nCell))
            }

        }
        h[x] <- 1
        # ### saving yearly timesteps for testing purposes
        # save(h, file = paste0(outputDir, "h_", y, ".RData"))
        ## storing harvested stands
        harv[[y]] <- h

        ####################### updating tsd
        tsd[h] <- 0
        
        age[[y]] <- tsd
        
       
        # ### saving yearly timesteps for testing purposes
        # save(tsd, file = paste0(outputDir, "tsd_", y, ".RData"))
        ###
        print("##############################################################")
        print("##############################################################")
        
        #######################
        ### aging landscape for next year
        tsd <- tsd + 1
       
        
    }
    
    fire <- stack(fire)
    harv <- stack(harv)
    age <- stack(age)
    density <- stack(density)

    save(fire, file = paste0(outputDir, "outputFire_", str_pad(i, nchar(nRep-1), pad = "0"), ".RData"))
    save(harv, file = paste0(outputDir, "outputHarvest_", str_pad(i, nchar(nRep-1), pad = "0"), ".RData"))
    save(age, file = paste0(outputDir, "outputTSD_", str_pad(i, nchar(nRep-1), pad = "0"), ".RData"))
    save(density, file = paste0(outputDir, "outputDensity_", str_pad(i, nchar(nRep-1), pad = "0"), ".RData"))
    
    print("##############################################################")
    print("##############################################################")
    print(paste0("Simulation #", i, " completed ", Sys.time()-t1))
    print("##############################################################")

}

stopCluster(cl)
