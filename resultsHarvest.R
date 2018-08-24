###################################################################################################
###################################################################################################
##### Visualizing fire simulations
##### Dominic Cyr, in collaboration with Tadeusz Splawinski, Sylvie Gauthier, and Jesus Pascual Puigdevall
rm(list = ls())
setwd("D:/regenFailureRiskAssessmentData_phase2/2018-08-23")
####################################################################################################
####################################################################################################
wwd <- paste(getwd(), Sys.Date(), sep = "/")
dir.create(wwd)
setwd(wwd)
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

uaf <- raster("../uaf.tif")
uaf_RAT <- read.csv("../uaf_RAT.csv")

####################################################################
####################################################################
######
### fetching compiled results
outputCompiled <- get(load("outputCompiledHarvest.RData"))

nSims <- nrow(distinct(outputCompiled, scenario, replicate))

### summarizing results, percentile & such
outputCompiled <- filter(outputCompiled, uaf != "total")



target <- outputCompiled %>%
    group_by(scenario, year, replicate) %>%
    summarise(target = sum(areaHarvestedTotal_ha)) %>%
    group_by() %>%
    summarise(target = max(target)) %>%
    as.numeric()

summaryHarvest <- outputCompiled %>%
    group_by(scenario, year, replicate) %>%
    summarise(areaHarvestedTotal_ha = sum(areaHarvestedTotal_ha)) %>%
    group_by(scenario, year) %>%
    summarise(p01HarvestProp = quantile(areaHarvestedTotal_ha, .01),
              p05HarvestProp = quantile(areaHarvestedTotal_ha, .05),
              p25HarvestProp = quantile(areaHarvestedTotal_ha, .25),
              p50HarvestProp = quantile(areaHarvestedTotal_ha, .5),
              p75HarvestProp = quantile(areaHarvestedTotal_ha, .75),
              p95HarvestProp = quantile(areaHarvestedTotal_ha, .95),
              p99HarvestProp = quantile(areaHarvestedTotal_ha, .99))


##############################################################################
### Plotting realized harvests 
#######
vars <- colnames(summaryHarvest)
vars <- vars[grep("HarvestProp", vars)]

percentile <- as.numeric(gsub("[^0-9]","", vars))
#varName <- paste0(p, "Harvest_ha")
df <- summaryHarvest
### reformatting harvest treatments for better readability
df <- melt(df, id.vars = c("scenario", "year"), 
                            measure.vars = vars,
           variable.name = "prob")
df$prob <- as.numeric(gsub("[^0-9]","", df$prob))
df$prob <- paste0(df$prob, "%")
df$prob <- factor(df$prob, levels = c("1%", "5%", "25%", "50%", "75%", "95%", "99%"))


m <- ggplot(df, aes(x = year + 2015, y = (value/target)*100,
                    linetype = prob)) +
    #facet_grid(coverType ~ scenario) +
    geom_line(size = 0.5) +
    scale_linetype_manual("Percentiles\n",
                      values =seq_along(df$prob)) +
    guides(linetype = guide_legend(reverse=T))


png(filename= paste0("harvestRealized.png"),
    width = 8, height = 6, units = "in", res = 600, pointsize=10)

options(scipen=999)

print(m + theme_dark() +
          
          theme(#legend.position="top", legend.direction="horizontal",
                legend.title = element_text(size = rel(0.85)),
                title = element_text(size = rel(0.85)),
                #plot.subtitle = element_text(size = rel(1)),
                plot.caption = element_text(size = rel(0.65))) +
          
          labs(title = "Analyse des récoltes réalisées",
               #subtitle = paste0(percentile, "e percentile"),
               subtitle = paste0("Chaque courbe représente la fraction du taux de récolte ciblé (", target, " ha/année) bel et bien récoltée\n",
                                 "pour un percentile donné au sein d'un ensemble de ",nSims, " simulations."),
               caption = paste0("Âge de récolte - Épinette noire: 90 ans\n",
                                "Pin gris: 76 ans\n",
                                "Cycle des feux - baseline: 104 ans\n",
                                "Min vieilles forêts (>=100 ans): 14%\n",
                                "Max régén. (< 20 ans): 35%"),
               x = "",
               y = "Fraction du taux de récolte ciblé récoltée (%)\n"))

dev.off()



### summarizing results, shortfall probs
shortfallDF <- outputCompiled %>%
    group_by(scenario, year, replicate) %>%
    summarise(areaHarvestedTotal_ha = sum(areaHarvestedTotal_ha)) %>%
    #group_by(scenario, year) %>%
    mutate(p50_shortfall = areaHarvestedTotal_ha < .50*target,
           p25_shortfall = areaHarvestedTotal_ha < .75*target,
           p10_shortfall = areaHarvestedTotal_ha < .90*target,
           p05_shortfall = areaHarvestedTotal_ha < .95*target) %>%
    group_by(scenario, replicate) %>%
    arrange(year) %>%
    mutate(p50_shortfall = cumsum(p50_shortfall)>=1,
           p25_shortfall = cumsum(p25_shortfall)>=1,
           p10_shortfall = cumsum(p10_shortfall)>=1,
           p05_shortfall = cumsum(p05_shortfall)>=1) %>%
    ungroup() %>%
    group_by(scenario, year) %>%
    summarise(p50_shortfall = sum(p50_shortfall)/n(),
              p25_shortfall = sum(p25_shortfall)/n(),
              p10_shortfall = sum(p10_shortfall)/n(),
              p05_shortfall = sum(p05_shortfall)/n())



#######

##############################################################################
### Plotting probability of shortfall 
#######

vars <- colnames(shortfallDF)
vars <- vars[grep("shortfall", vars)]

riskTol <- paste0(as.numeric(gsub("[^0-9]","", vars)), "%")
df <- shortfallDF
## reformating in tall form
require(reshape2)
df <- melt(shortfallDF, id.vars = c("scenario", "year"),
           measure.vars = vars)


df$variable <- as.numeric(gsub("[^0-9]", "",  df$variable))
df$variable <- paste0(df$variable, "%")
df$variable <- factor(df$variable, levels = c("5%", "10%", "25%", "50%"))


m <- ggplot(df, aes(x = year + 2015, y = value*100,
                    linetype = variable)) +
    geom_line(size = 0.5) +
    scale_linetype_manual("Écart toléré\n",
                    values = seq_along(levels(df$variable)))


png(filename= paste0("harvestShortfall.png"),
    width = 8, height = 6, units = "in", res = 600, pointsize=10)
options(scipen=999)

print(m + theme_dark() +
          
          theme(#legend.position="top", legend.direction="horizontal",
                legend.title = element_text(size = rel(0.85)),
                title = element_text(size = rel(0.85)),
                #plot.subtitle = element_text(size = rel(1)),
                plot.caption = element_text(size = rel(0.65))) +
          
          labs(title = "Analyse de risque de rupture d'approvisionnement",
               #subtitle = paste0(percentile, "e percentile"),
               subtitle = paste0("Probabilité de ne pas atteindre la cible de récolte au moins une fois en fonction de différents niveaux de\n",
                                 "tolérance par rapport à cette cible - estimé sur la base d'un ensemble de ", nSims, " simulations."),
               caption = paste0("Âge de récolte - Épinette noire: 90 ans\n",
                                "Pin gris: 76 ans\n",
                                "Cycle des feux - baseline: 104 ans\n",
                                "Min vieilles forêts (>=100 ans): 14%\n",
                                "Max régén. (< 20 ans): 35%"),
               x = "",
               y = "Probabilité de rupture\nd'approvisionnement (%)\n"))


dev.off()


