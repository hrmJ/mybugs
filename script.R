#!/usr/bin/env Rscript

library(rjags)
library(coda)
library(mcmcplots)

if(!exists("aineisto")){
    aineisto <- readRDS("../koko_aineisto.rds")
}


#Muita mahdollisia muuttujia: funct, clausestatus 

observations=xtabs(~ lang + morph + ref + location, data=aineisto)
totals=xtabs(~ lang + morph + ref, data=aineisto)


dataList  <-  list(observations=observations,
                   totals=totals,
                   nLang=length(unique(aineisto$lang)),
                   nMorph=length(unique(aineisto$morph)),
                   nRef=length(unique(aineisto$ref)),
                   Nlocations = 4)

monitor <- c("b.ref")

jagsModel <- jags.model("model.bugs", data=dataList, n.chains = 1, n.adapt = 4000)
update(jagsModel, n.iter=4000)
post <- coda.samples(jagsModel, variable.names=monitor, n.iter=4000, thin=1) 
show(summary(post)$statistics)
#mcmcplot(post)
#pannaan talteen probsit, niin saadaan 3-ulotteinen taulukko todennäköisyydet
