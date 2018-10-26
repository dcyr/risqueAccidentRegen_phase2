###################################################################################################
###################################################################################################
##### Visualizing the evolution volumes at 120 y.old (indicator of landscape productivity)
##### Dominic Cyr, in collaboration with Tadeusz Splawinski, Sylvie Gauthier, and Jesus Pascual Puigdevall
rm(list = ls()[-which(ls() %in% c("sourceDir", "scenario"))])
# setwd("D:/regenFailureRiskAssessmentData_phase2/2018-10-23")
# wwd <- paste(getwd(), Sys.Date(), sep = "/")
# dir.create(wwd)
# setwd(wwd)
#################
require(raster)
require(ggplot2)
require(dplyr)
require(reshape2)
initYear <- 2015
####################################################################
####################################################################
######
studyArea <- raster("../studyArea.tif")
subZone <- raster("../subZones.tif")
subZone_RAT <- read.csv("../subZones_RAT.csv")
##

# uaf <- raster("../uaf.tif")
# uaf_RAT <- read.csv("../uaf_RAT.csv")

####################################################################
####################################################################
######
### fetching compiled results
outputCompiled <- get(load("outputCompiledVolAt120Cls.RData"))

nSims <- nrow(distinct(outputCompiled, scenario, replicate))

### summarizing results, percentile & such
# outputCompiled <- filter(outputCompiled, uaf != "total")
outputCompiled[is.na(outputCompiled$volAt120Cls), "volAt120Cls"] <- "[0,50)"



df <- outputCompiled %>%
    group_by(scenario, year, coverTypes, subZone, volAt120Cls) %>%
    #summarise(areaHarvestedTotal_ha = sum(areaHarvestedTotal_ha)) %>%
    #group_by(scenario, year) %>%
    summarise(#p01HarvestProp = quantile(areaHarvestedTotal_ha, .01),
              #p05HarvestProp = quantile(areaHarvestedTotal_ha, .05),
              p25VolAt120Area_ha = quantile(area_ha, .025),
              p50VolAt120Area_ha = quantile(area_ha, .5),
              p75VolAt120Area_ha = quantile(area_ha, .975)
              #p95HarvestProp = quantile(areaHarvestedTotal_ha, .95),
              #p99HarvestProp = quantile(areaHarvestedTotal_ha, .99)
              )

#### testing outputs
require(ggplot2)


png(filename= paste0("volAt120.png"),
    width = 8, height = 4, units = "in", res = 600, pointsize=10)

options(scipen=999)


ggplot(data = df, aes(x = 2015 + year, y = p50VolAt120Area_ha, colour = volAt120Cls)) +
    geom_line() +
   # ?geom_ribbon()
    geom_ribbon(aes(ymin = p25VolAt120Area_ha, ymax=p75VolAt120Area_ha,
                        x=  2015 + year, fill = df$volAt120Cls), alpha = 0.25, colour = NA) +
    facet_grid(coverTypes ~ subZone, scales = "free_y") +
    labs(x = "",
         y = "Area (ha)") +
    scale_colour_manual("Vol. at 120 y.old",values= c("skyblue4", "darkgreen", "indianred4")) +
    scale_fill_manual("I.C.95% ",values= c("skyblue4", "darkgreen", "indianred4")) +
    theme(strip.text.x = element_text(size = 7),
           axis.text.x = element_text(angle = 45, hjust = 1)) 

dev.off()



