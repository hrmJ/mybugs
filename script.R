#!/usr/bin/env Rscript

library(rjags)
library(runjags)
library(coda)
library(mcmcplots)
library(ggmcmc)
source("functions.R")

if(!exists("aineisto")){
    aineisto <- readRDS("/home/juho_harme/phdmanuscript/monograph/data/dumps/vaihe1df.rds")
}


#Muita mahdollisia muuttujia: funct, clausestatus 

observations=xtabs(~ lang + morph + ref + funct + clausestatus + subjtype + objtype + location, data=aineisto)
totals=xtabs(~ lang + morph + ref + funct + clausestatus + subjtype + objtype, data=aineisto)


dataList  <-  list(observations=observations,
                   totals=totals,
                   nLang=length(unique(aineisto$lang)),
                   nMorph=length(unique(aineisto$morph)),
                   nRef=length(unique(aineisto$ref)),
                   nFunct=length(unique(aineisto$funct)),
                   nClausestatus=length(unique(aineisto$clausestatus)),
                   Nlocations = 4)

monitor <- c("b.funct","b.ref","morph","lang.morph","b.lang.ref","b.lang.funct","subjtype",
             "std.lang"," std.morph"," std.funct"," std.ref"," std.lang.morph"," std.lang.ref"," std.lang.funct",
             "std.subjtype", "std.lang.subjtype")


con <- file("log.txt")
sink(con, append=TRUE)

x <- Sys.time()
show(x)

result <- run.jags("model.bugs", monitor = monitor, data = dataList, 
                     summarise = TRUE, interactive = FALSE, 
                     method = "parallel",adapt=50000,
                     jags.refresh = 60) #refresh: kuinka usein (sekunneissa) katsotaan, onko edistystä tullut

show(Sys.time()-x)

sink() 
close(con)

#time.adapt <- system.time(jagsModel <- jags.model("model.bugs", data=dataList, n.chains = 1, n.adapt = 200000))
#show(time.adapt)

#time.update <- system.time(update(jagsModel, n.iter=2000))
#show(time.update)
#time.sample <- system.time(post <- coda.samples(jagsModel, variable.names=monitor, n.iter=20000, thin=1))
#
#sum.saved <- summary(post)$statistics
#show(sum.saved)
#show(time.update)
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

#saveRDS(post,"../phdmanuscript/monograph/data/dumps/hierarchical_dirichlecht_morph-ref-funct-clausestatus-subjtype-objtype.rds")


