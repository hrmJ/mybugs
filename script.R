#!/usr/bin/env Rscript

library(rjags)
library(runjags)
library(coda)
library(mcmcplots)
library(ggmcmc)

if(!exists("aineisto")){
    aineisto <- readRDS("../vaihe1df.rds")
}
source("../functions.R")

aineisto$location[aineisto$location %in% c("S2","S3")] <- "S2/S3"

#Muita mahdollisia muuttujia: funct, clausestatus 

observations=xtabs(~ lang + morph + ref + funct + clausestatus + location, data=aineisto)
totals=xtabs(~ lang + morph + ref + funct + clausestatus, data=aineisto)


dataList  <-  list(observations=observations,
                   totals=totals,
                   nLang=length(unique(aineisto$lang)),
                   nMorph=length(unique(aineisto$morph)),
                   nRef=length(unique(aineisto$ref)),
                   nFunct=length(unique(aineisto$funct)),
                   nClausestatus=length(unique(aineisto$clausestatus)),
                   Nlocations = length(unique(aineisto$location)))

monitor <- c("b.funct","b.ref","morph","lang.morph","b.lang.ref","b.lang.funct","b.clausestatus", "std.clausestatus",
             "std.lang","std.morph"," std.funct"," std.ref","std.lang.morph","std.lang.ref","std.lang.funct",
             "std.lang.clausestatus","b.lang.clausestatus")



con <- file("~/public_html/currentlog.txt")
sink(con, append=TRUE)
x <- Sys.time()
show(x)

RunJagsModel <- run.jags(data=dataList, monitor=monitor, model="model.bugs",
                            n.chains=2, adapt=40000, burnin=5000, thin=1,
                            method="parallel", sample=20000)

saveRDS(RunJagsModel,"THREELOC_runjags_20000_withclausestatus.rds")
saveRDS(RunJagsModel,"~/phdmanuscript/monograph/data/dumps/THREELOC_runjags_20000_withclausestatus.rds")
print("SAVED.")


sink() 

close(con)

