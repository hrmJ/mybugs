#!/usr/bin/env Rscript

library(rjags)
library(coda)
library(mcmcplots)

if(!exists("aineisto")){
    aineisto <- readRDS("koko_aineisto.rds")
}


#Muita mahdollisia muuttujia: funct, clausestatus 

observations=xtabs(~ lang + morph + location, data=aineisto)
totals=xtabs(~ lang + morph, data=aineisto)


dataList  <-  list(observations=observations,
                   totals=totals,
                   nLang=length(unique(aineisto$lang)),
                   nMorph=length(unique(aineisto$morph)),
                   Nlocations = 4)

monitor <- c("morph.lang")

jagsModel <- jags.model("model_odell.bugs", data=dataList, n.chains = 1, n.adapt = 500)
update(jagsModel, n.iter=500)
post <- coda.samples(jagsModel, variable.names=monitor, n.iter=1000, thin=1) 
show(summary(post)$statistics)
#mcmcplot(post)
#pannaan talteen probsit, niin saadaan 3-ulotteinen taulukko todennäköisyydet
