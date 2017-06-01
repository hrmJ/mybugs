#!/usr/bin/env Rscript

library(rjags)
library(runjags)
library(coda)
library(mcmcplots)

if(!exists("aineisto")){
    #aineisto <- readRDS("../vaihe1df.rds")
    aineisto <- readRDS("~/phdmanuscript/monograph/data/dumps/vaihe1df.rds")
}
source("functions.R")

aineisto$location <- as.character(aineisto$location)
aineisto$location[aineisto$location %in% c("S2","S3")] <- "S2/S3"
aineisto$location <- as.factor(aineisto$location)

#Muita mahdollisia muuttujia: funct, clausestatus 

observations=xtabs(~ lang + pos + morph +  funct + clausestatus + location, data=aineisto)
totals=xtabs(~ lang + pos + morph +  funct + clausestatus, data=aineisto)


                   nLang=length(unique(aineisto$lang)),
                   nMorph=length(unique(aineisto$morph)),
                   nPos=length(unique(aineisto$pos)),
                   nFunct=length(unique(aineisto$funct)),
                   nClausestatus=length(unique(aineisto$clausestatus)),
                   Nlocations = length(unique(aineisto$location)))

monitor <- c("lang",     "b.morph",        "pos",           "b.funct",         "b.clausestatus" ,
             "std.lang", "std.morph",      "std.pos",       "std.funct",       "std.clausestatus" ,
                         "b.lang.morph",   "lang.pos",      "b.lang.funct",    "b.lang.clausestatus" ,
                         "std.lang.morph", "std.lang.pos" , "std.lang.funct" , "std.lang.clausestatus")



#con <- file("~/public_html/currentlog.txt")
#sink(con, append=TRUE)
#x <- Sys.time()
#show(x)

RunJagsModel <- run.jags(data=dataList, monitor=monitor, model="model.bugs",
                            n.chains=2, adapt=40000, burnin=10000, thin=10,
                            method="parallel", sample=100000)

saveRDS(RunJagsModel,"withpos_100k.rds")
saveRDS(RunJagsModel,"~/phdmanuscript/monograph/data/dumps/withpos_100k.rds")
print("SAVED.")


#sink() 

#close(con)


post <- as.mcmc.list(RunJagsModel)
mcmcplot(post,dir="~/public_html/jags")


