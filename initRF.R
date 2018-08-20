####################################################################################################
####################################################################################################
###### Preparation of fire regime parameters and study area + subzones
###### Dominic Cyr, in collaboration with Tadeusz Splawinski and Sylvie Gauthier
rm(list = ls())
home <- path.expand("~")
home <- gsub("/Documents", "", home) # necessary on my Windows machine
setwd(paste(home, "Sync/Travail/ECCC/regenFailureRiskAssessment_phase2", sep ="/"))
####################################################################################################
####################################################################################################
wwd <- paste(getwd(), Sys.Date(), sep = "/")
dir.create(wwd)
setwd(wwd)

require(rgdal)
require(raster)
require(dplyr)


####################################################################################################
#########  fetching source data
inputDir <- "../gis/rf"
####################################################################################################

### loading reference raster
studyArea <- raster("../data/studyArea.tif")
## fetching latitude values (for later)
studyAreaLL <- projectRaster(studyArea, crs = CRS("+init=epsg:4326")) ## lat long WGS 84
lat <- studyAreaLL
lat[] <- coordinates(lat)[,"y"]
lat <-  projectRaster(lat, studyArea)


### fetching ecological district-level variables
ecoDist <- readOGR(dsn = inputDir, layer = "distEcolVar")
#ecoDist <- readOGR(dsn = inputDir, layer = "distEcol567")
ecoDist <- spTransform(ecoDist, CRSobj = crs(studyArea))
ecoDist <- crop(ecoDist, studyArea)
## converting some variable back to numeric
ecoDist$abonEric <- as.numeric(as.character(ecoDist$abonEric))
ecoDist$degJour <- as.numeric(as.character(ecoDist$degJour))
ecoDist$saiCro <- as.numeric(as.character(ecoDist$saiCro))
ecoDist$IndSeche <- as.numeric(as.character(ecoDist$IndSeche))




####################################################################################################
#########  constants
####################################################################################################

#############
## ericaceous abundance
ericaceous <-  rasterize(ecoDist, studyArea, field = "abonEric")
ericaceous[is.na(studyArea)] <- NA

#############
## growing degree-days (above 5 degrees )
gddOver5 <- rasterize(ecoDist, studyArea, field = "degJour")
gddOver5[is.na(studyArea)] <- NA

############
## growing season (days)
growSeas_days  <- rasterize(ecoDist, studyArea, field = "saiCro")
growSeas_days[is.na(studyArea)] <- NA

############
## aridity index (de Martonne) - Supposed to be much indArid than these values, need to validate
AI_deMartonne  <- rasterize(ecoDist, studyArea, field = "indArid")
AI_deMartonne[is.na(studyArea)] <- NA

############
## proPot (what is this?)
prodPot  <- rasterize(ecoDist, studyArea, field = "prodPot")
prodPot[is.na(studyArea)] <- NA

############
## prePot (what is this?)
preTot  <- rasterize(ecoDist, studyArea, field = "preTot")
preTot[is.na(studyArea)] <- NA

#############
## elevation (+ aspect and slope, may not be necessary in the end...)
dem <- new("GDALReadOnlyDataset", paste(inputDir, "studyarea_dem", sep = "/"))
dem <- asSGDF_GROD(dem)
dem <- raster(dem)
dem <- projectRaster(dem, studyArea) ## lat long WGS 84
dem <- round(dem)

# asp and slo may not be necessary if the 'spatialEco' package is used to compute HLI
asp <- new("GDALReadOnlyDataset", paste(inputDir, "studyarea_asp", sep = "/"))
asp <- asSGDF_GROD(asp)
asp <- raster(asp)
asp <- projectRaster(asp, studyArea) ## lat long WGS 84

slo <- new("GDALReadOnlyDataset", paste(inputDir, "studyarea_slo", sep = "/"))
slo <- asSGDF_GROD(slo)
slo <- raster(slo)
slo <- projectRaster(slo, studyArea) ## lat long WGS 84

## computing HLI (see also other method from package 'spatialEco')
demLL <- projectRaster(dem, crs = CRS("+init=epsg:4326")) ## lat long WGS 84
## Heat Load Index (McCune and Keon 2002)
asp1 <- 180-abs(asp-180)   # aspect folded to 0-180 degrees
hli1 <-0.339+(0.808*cos(lat/360*2*pi)*cos(slo/360*2*pi)) -
    (0.196*sin(lat/360*2*pi)*sin(slo/360*2*pi))-(0.482*sin(asp1/360*2*pi)*sin(slo/360*2*pi))
hli <- hli1
hli <- round(hli, 4)

### found some discrepancies between the two methods... to be resolved
### Also, some values are NA's (-1 degree asp?)
# require(spatialEco)
# hli2 <- hli(demLL)
# png(filename = "hli_manuel.png",
#         width = 1200, height = 800, units = "px", res = 300, pointsize = 6,
#         bg = "white")
# 
# plot(hli1)
# 
# dev.off()

# png(filename = "hli_spatialEcoPackage.png",
#     width = 1200, height = 800, units = "px", res = 300, pointsize = 6,
#     bg = "white")
# 
# plot(hli2, main = "Heat Load Index - 'SpatialEco' package")
# 
# dev.off()

# saving rasters



#############
## surficial deposits - Reclassified by Splawinski
### Maybe should be reorganized... (hydric sands?)
### In fact, why not keeping them separate?


depositCodes <- list(shallow = c("R", "R1A", "R4GA", "R7T",
                              "R8E", "8E"),
                     till = c("1A", "1AA","1AAM", "1AAY",
                              "1AB", "1AD", "1AM", "1AY",
                              "1BC","1BD", "1BF", "1BG",
                              "1BI", "1BN", "1BP", "1BT"),
                     sand = c("2A", "2AE", "2AK", "2AT",
                              "2BD", "2BE", "3AE", "3AN",
                              "4GS", "4P","9S"),
                     clay = c("4GA", "4GAM", "4GAY"),
                     organic = c("7E", "7T", "7TM", "7TY"))
drainageCodes <- list(Mesic = c("00", "10", "11", "16",
                                "20", "21", "23", "24",
                                "30", "31"),
                      Hydric = c("40", "41", "42", "43",
                                 "44", "50", "51", "52",
                                 "53", "54", "60", "61",
                                 "63"))
# 
# 
surfDep <- x <- as.character(forestInventory$DEP_SUR)
drain <- as.character(forestInventory$CL_DRAI)

for (dep in names(depositCodes)) {
    depIndex <- which(surfDep %in% depositCodes[[dep]])
    x[depIndex] <- dep
    if (dep %in% c("clay", "sand")) {
        for (dra in names(drainageCodes)){
            draIndex <-  which(drain %in% drainageCodes[[dra]])
            index <- intersect(depIndex, draIndex)
            x[index] <- paste0(dep, dra)   
        }
    }
}

surfDepLevels <- c("shallow", "sandMesic", "sandHydric", "till", "clayMesic", "clayHydric", "organic")
surfDep <- factor(x, levels = surfDepLevels)
surfDepRat <- data.frame(ID = seq_along(surfDepLevels), surfDep = surfDepLevels)

forestInventory$surfDep <- surfDep

## rasterizing this new factor
surfDep  <- rasterize(forestInventory, studyArea, field = "surfDep")
surfDep[is.na(studyArea)] <- NA
write.csv(surfDepRat, file = "surfDepRat.csv", row.names = F)


writeRaster(dem, file = "dem.tif", overwrite = T)
writeRaster(hli, file = "hli.tif", overwrite = T)
writeRaster(ericaceous, file = "ericaceous.tif", overwrite = T)
writeRaster(gddOver5, file = "gddOver5.tif", overwrite = T)
writeRaster(growSeas_days, file = "growSeas_days.tif", overwrite = T)
writeRaster(AI_deMartonne, file = "AI_deMartonne.tif", overwrite = T)
writeRaster(surfDep, file = "surfDep.tif", overwrite = T)
writeRaster(prodPot, file = "prodPot.tif", overwrite = T)
writeRaster(preTot, file = "preTot.tif", overwrite = T)

####################################################################################################
#########  variables (the inventory takes a while to process)
####################################################################################################


#############
###  initial density
densCls <- c(A = "AB", B = "AB", C = "C", D = "D", I = "AB")
densLevels <- unique(densCls)
CL_DENS_NEW <- densCls[match(forestInventory$CL_DENS, names(densCls))]
forestInventory$CL_DENS_NEW <- factor(CL_DENS_NEW, levels = densLevels)
#
dens <- rasterize(forestInventory, studyArea, field = "CL_DENS_NEW")
dens[is.na(studyArea)] <- NA
# plot(dens)
# foo <- dens

### attaching level description
dens <- ratify(dens)
rat <- levels(dens)[[1]]
rat[,"dens"] <- densLevels
### aggregating levels
write.csv(rat, file = "densRat.csv", row.names = F)
## setting initial density for young stands (to be refined)

### loading covertypes
coverTypes <- raster("../data/coverTypes.tif")
coverTypesDF <- get(load("../data/coverTypesDf.RData"))
coverTypesRat <- distinct(coverTypesDF[, c("ID", "descrip")])
coverTypesProd <- c("EN", "PG", "F", "R")
write.csv(coverTypesRat, file = "coverTypesRat.csv", row.names = F)
### creating frequency table 
freqTable <- table(values(coverTypes), values(dens), useNA = "ifany")
rownames(freqTable) <- c(as.character(coverTypesRat[order(coverTypesRat$ID),"descrip"]), "NA")
colnames(freqTable) <- c(as.character(rat[order(rat$ID),"dens"]), "NA")
freqTable <- as.data.frame(freqTable) %>%
    group_by(Var1, Var2) %>%
    summarize(Freq = sum(Freq)) %>%
    filter(Var1 %in% coverTypesProd)

for (i in coverTypesProd) {
    d <- coverTypesRat[coverTypesRat$descrip == i, "ID"]
    prob <- freqTable %>%
        filter(Var1 == i &
                   Var2 != "NA")
    prob <- as.data.frame(prob)
    index <- which(values(coverTypes) == d &
                       is.na(values(dens)))
    dens[index] <- sample(rat$ID, size = length(index), prob = prob[match(prob$Var2, rat$dens),"Freq"], replace = T)
}
## some unproductive sites had density values (?), setting them to NA
dens[coverTypes %in% coverTypesRat[match(coverTypesProd, coverTypesRat$descrip), "ID"] == F] <- NA
writeRaster(dens, file = "dens.tif", overwrite = T)
write.csv(rat, file = "densRat.csv", row.names = F)

