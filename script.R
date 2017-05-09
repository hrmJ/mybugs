#!/usr/bin/env Rscript

library(rjags)
library(coda)
library(mcmcplots)
library(ggmcmc)
source("functions.R")

if(!exists("aineisto")){
    aineisto <- readRDS("../koko_aineisto.rds")
}


#Muita mahdollisia muuttujia: funct, clausestatus 

observations=xtabs(~ lang + morph + ref + funct + location, data=aineisto)
totals=xtabs(~ lang + morph + ref + funct, data=aineisto)


dataList  <-  list(observations=observations,
                   totals=totals,
                   nLang=length(unique(aineisto$lang)),
                   nMorph=length(unique(aineisto$morph)),
                   nRef=length(unique(aineisto$ref)),
                   nFunct=length(unique(aineisto$funct)),
                   Nlocations = 4)

monitor <- c("funct")


jagsModel <- jags.model("model.bugs", data=dataList, n.chains = 1, n.adapt = 500)
update(jagsModel, n.iter=500)
post <- coda.samples(jagsModel, variable.names=monitor, n.iter=1000, thin=1) 
show(summary(post)$statistics)
#mcmcplot(post)
#pannaan talteen probsit, niin saadaan 3-ulotteinen taulukko todennäköisyydet



#intercepts <- GetLabelNamesForGgMcmc("b.lang.ref",list(levels(aineisto$lang),levels(aineisto$ref), levels(aineisto$loc)))
#intercepts <- GetLabelNamesForGgMcmc("lang.morph",list(levels(aineisto$lang),levels(aineisto$morph), levels(aineisto$loc)))
#intercepts <- GetLabelNamesForGgMcmc("funct",list(levels(aineisto$funct), levels(aineisto$loc)))
#intercepts$lang <- "fi"
#intercepts$lang[grepl("ru,",intercepts$Label)] <- "ru"
#intercepts$loc <- "S1"
#intercepts$loc[grepl("S2",intercepts$Label)] <- "S2"
#intercepts$loc[grepl("S3",intercepts$Label)] <- "S3"
#intercepts$loc[grepl("S4",intercepts$Label)] <- "S4"

#
#ggs_caterpillar(ggs(post, par_labels=intercepts),family="ADV") + aes(color=lang) + facet_wrap(~ loc)
#ggs_caterpillar(ggs(post, par_labels=intercepts)) 


