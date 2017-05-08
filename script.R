#!/usr/bin/env Rscript

library(rjags)
library(coda)
library(mcmcplots)
library(ggmcmc)

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

monitor <- c("lang.morph")


jagsModel <- jags.model("model.bugs", data=dataList, n.chains = 2, n.adapt = 15000)
update(jagsModel, n.iter=20000)
post <- coda.samples(jagsModel, variable.names=monitor, n.iter=20000, thin=1) 
#show(summary(post)$statistics)
#mcmcplot(post)
#pannaan talteen probsit, niin saadaan 3-ulotteinen taulukko todennäköisyydet



#intercepts <- GetLabelNamesForGgMcmc("b.lang.ref",list(levels(aineisto$lang),levels(aineisto$ref), levels(aineisto$loc)))
intercepts <- GetLabelNamesForGgMcmc("lang.morph",list(levels(aineisto$lang),levels(aineisto$morph), levels(aineisto$loc)))
intercepts$lang <- "fi"
intercepts$lang[grepl("ru,",intercepts$Label)] <- "ru"

intercepts$loc <- "S1"
intercepts$loc[grepl("S2",intercepts$Label)] <- "S2"
intercepts$loc[grepl("S3",intercepts$Label)] <- "S3"
intercepts$loc[grepl("S4",intercepts$Label)] <- "S4"

#
ggs_caterpillar(ggs(post, par_labels=intercepts),family="ADV") + aes(color=lang) + facet_wrap(~ loc)


