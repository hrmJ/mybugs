#!/usr/bin/env Rscript

library(rjags)
library(runjags)
library(coda)
library(mcmcplots)
library(ggmcmc)

if(!exists("aineisto")){
    #aineisto <- readRDS("../vaihe1df.rds")
    aineisto <- readRDS("~/phdmanuscript/monograph/data/dumps/vaihe1df.rds")
}
source("functions.R")

aineisto$location <- as.character(aineisto$location)
aineisto$location[aineisto$location %in% c("S2","S3")] <- "S2/S3"
aineisto$location <- as.factor(aineisto$location)

#Muita mahdollisia muuttujia: funct, clausestatus 

observations=xtabs(~ lang + pos +  funct + clausestatus + location, data=aineisto)
totals=xtabs(~ lang + pos +  funct + clausestatus, data=aineisto)


dataList  <-  list(observations=observations,
                   totals=totals,
                   nLang=length(unique(aineisto$lang)),
                   nMorph=length(unique(aineisto$pos)),
                   nFunct=length(unique(aineisto$funct)),
                   nClausestatus=length(unique(aineisto$clausestatus)),
                   Nlocations = length(unique(aineisto$location)))

monitor <- c("b.funct","pos","lang.pos","b.lang.funct","b.clausestatus", "std.clausestatus",
             "std.lang","std.pos","std.funct","std.lang.pos","std.lang.funct",
             "std.lang.clausestatus","b.lang.clausestatus")



con <- file("~/public_html/currentlog.txt")
sink(con, append=TRUE)
x <- Sys.time()
show(x)

RunJagsModel <- run.jags(data=dataList, monitor=monitor, model="model.bugs",
                            n.chains=2, adapt=40000, burnin=10000, thin=1,
                            method="parallel", sample=30000)

saveRDS(RunJagsModel,"simplified3LOC_runjags_20000.rds")
saveRDS(RunJagsModel,"~/phdmanuscript/monograph/data/dumps/simplified3LOC_runjags_20000.rds")
print("SAVED.")


sink() 

close(con)


post <- as.mcmc.list(RunJagsModel)
extend.jags(RunJagsModel,sample=4000)
mcmcplot(post,dir="~/public_html/jags")


