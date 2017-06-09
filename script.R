#!/usr/bin/env Rscript

library(rjags)
library(runjags)
library(coda)
library(mcmcplots)

if(!exists("aineisto")){
    aineisto <- readRDS("vaihe1df.rds")
}
source("functions.R")

aineisto$location <- as.character(aineisto$location)
aineisto$location[aineisto$location %in% c("S2","S3")] <- "S2/S3"
aineisto$location <- as.factor(aineisto$location)

#Muita mahdollisia muuttujia: funct, clausestatus 

observations=xtabs(~ lang + pos + corpustype + morph +  funct + clausestatus + location, data=aineisto)
totals=xtabs(~ lang + pos + corpustype + morph +  funct + clausestatus, data=aineisto)


dataList  <-  list(observations=observations,
                   totals=totals,
                   nLang=length(unique(aineisto$lang)),
                   nMorph=length(unique(aineisto$morph)),
                   nPos=length(unique(aineisto$pos)),
                   nCorpus=length(unique(aineisto$corpustype)),
                   nFunct=length(unique(aineisto$funct)),
                   nClausestatus=length(unique(aineisto$clausestatus)),
                   Nlocations = length(unique(aineisto$location)))

monitor <- c("lang",     "b.morph",        "pos",           "b.funct",         "b.clausestatus" , "corpus",
             "std.lang", "std.morph",      "std.pos",       "std.funct",       "std.clausestatus" , "std.corpus",
                         "b.lang.morph",   "lang.pos",      "b.lang.funct",    "b.lang.clausestatus" , "lang.corpus",
                         "std.lang.morph", "std.lang.pos" , "std.lang.funct" , "std.lang.clausestatus", "std.lang.corpus")



con <- file(paste("c:Users/jh89963.STAFF/Dropbox/currentlog.txt"))
sink(con, append=TRUE)
show(Sys.time())

RunJagsModel <- run.jags(data=dataList, monitor=monitor, model="model.bugs",
                            n.chains=2, adapt=100000, burnin=50000, thin=9,
                            method="parallel", sample=80000)

post <- as.mcmc.list(RunJagsModel)
mcmcplot(post)
saveRDS(RunJagsModel,"withpos_100k.rds")
saveRDS(RunJagsModel,"c:Users/jh89963.STAFF/Dropbox/withcorpus_800k.rds")
print("SAVED.")

sink() 
close(con)




