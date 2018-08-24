####################################################################
####################################################################
### Random Forest prediction test - visualisation of outputs
####################################################################
####################################################################
rm(list = ls())
home <- path.expand("~")
home <- gsub("/Documents", "", home) # necessary on my Windows machine
setwd(paste(home, "Sync/Travail/ECCC/regenFailureRiskAssessment_phase2", sep ="/"))
####################################################################################################
####################################################################################################
wwd <- paste(getwd(), Sys.Date(), sep = "/")
dir.create(wwd)
setwd(wwd)

require(tidyr)
require(ggplot2)
require(dplyr)

output <- read.csv("../compiledOutputs/outputCompiledDensity_noAdHoc.csv")

x <- output %>%
    group_by(timestep, densityClass) %>%
    summarize(p50 = quantile(area_ha, 0.5),
              p25 = quantile(area_ha, 0.25),
              p75 = quantile(area_ha, 0.75))
    

x <- gather(as.data.frame(x),
            key = "percentile",
            value = "area_ha",
            -timestep, -densityClass,
            convert = T)
options(scipen=999)
### 
p <- ggplot(x, aes(x = timestep + 2015, y = area_ha, group = percentile, ID = percentile, color = percentile)) +
    geom_line(aes(linetype = percentile)) +
    theme_dark() +
    facet_wrap( ~ densityClass, scales = "free") +
    labs(title="Superficies occupées par chaque classe de densité (1000 simulations)",
         subtitle = "Random Forest seulement", 
         #subtitle = "Jeunes: transitions ad hoc; mature: Random Forest", 
         y="Superficie (ha)\n",
         x="") +
    scale_linetype_manual(values=c(p25 = 3, p50 = 1, p75 = 3)) +
    scale_colour_manual(values=c(p25 = "yellow", p50 = "lightblue", p75 = "yellow"))

png(filename=paste0("regenDens_AreaTotal_noAdHocTransitions.png"),
    width = 8, height = 5, units="in", res=600)
print(p)
dev.off()


####################################################################
####################################################################
#### Productif vs Improductif

x <- output %>%
    mutate(productif = densityClass %in% c("AB", "C", "D")) %>%
    mutate(productif = ifelse(productif == T, "Productif", "Improductif")) %>%
    group_by(timestep, productif) %>%
    summarize(p50 = quantile(sum(area_ha), 0.5),
              p25 = quantile(sum(area_ha), 0.25),
              p75 = quantile(sum(area_ha), 0.75))


x <- gather(as.data.frame(x),
            key = "percentile",
            value = "area_ha",
            -timestep, -productif,
            convert = T)
x$productif <- factor(x$productif, levels = c("Productif", "Improductif"))

options(scipen=999)
### 
p <- ggplot(x, aes(x = timestep + 2015, y = area_ha, group = percentile, ID = percentile, color = percentile)) +
    geom_line(aes(linetype = percentile)) +
    theme_dark() +
    facet_wrap( ~ productif, scales = "fixed") +
    labs(title="Superficies occupées par chaque classe de densité (1000 simulations)",
         subtitle = "Random Forest seulement", 
         #subtitle = "Jeunes: transitions ad hoc; mature: Random Forest", 
         y="Superficie (ha)\n",
         x="") +
    scale_linetype_manual(values=c(p25 = 3, p50 = 1, p75 = 3)) +
    scale_colour_manual(values=c(p25 = "yellow", p50 = "lightblue", p75 = "yellow"))

png(filename=paste0("regenDens_AreaTotal_noAdHocTransitions.png"),
    width = 8, height = 5, units="in", res=600)
print(p)
dev.off()