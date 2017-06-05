#!/usr/bin/env Rscript

library(rjags)
#library(jagsUI)
library(runjags)
library(coda)
library(mcmcplots)
#library(ggmcmc)
source("functions.R")

if(!exists("aineisto")){
    aineisto <- readRDS("vaihe1df.rds")
}


#Muita mahdollisia muuttujia: funct, clausestatus 

observations=xtabs(~ lang + morph + ref + funct + subjtype + location, data=aineisto)
totals=xtabs(~ lang + morph + ref + funct + subjtype , data=aineisto)


dataList  <-  list(observations=observations,
                   totals=totals,
                   nLang=length(unique(aineisto$lang)),
                   nMorph=length(unique(aineisto$morph)),
                   nRef=length(unique(aineisto$ref)),
                   nFunct=length(unique(aineisto$funct)),
                   Nlocations = 4)

monitor <- c("b.funct","b.ref","morph","lang.morph","b.lang.ref","b.lang.funct","subjtype",
             "std.lang"," std.morph"," std.funct"," std.ref"," std.lang.morph"," std.lang.ref"," std.lang.funct",
             "lang.subjtype",
             "std.subjtype", "std.lang.subjtype")



x <- Sys.time()
show(x)

con <- file(paste("c:Users/jh89963.STAFF/Dropbox/currentlog.txt"))
sink(con, append=TRUE)

jagsModel <- jags.model("model.bugs", data=dataList, n.chains = 1, n.adapt = 40000)

show(time.adapt)


#autojags.model <- autojags(data=dataList, parameters.to.save=monitor, model.file="model.bugs",
#                            n.chains=2, n.adapt=50000, iter.increment=1000, n.burnin=500, n.thin=1,
#                            save.all.iter=FALSE, parallel=TRUE,  DIC=FALSE,
#                            store.data=FALSE, codaOnly=c("location","ref","funct","refb0","functb0","lang.raw"),
#                            max.iter=100000, verbose=TRUE)
#
show(Sys.time()-x)
sink()
close(con)
#saveRDS(,"c:Users/jh89963.STAFF/Dropbox/result.rds")

saveRDS(jagsModel,"adapted_40000_noclausestatus.rds")
saveRDS(post,"1000samples_from_40000_noclausestatus.rds")

post <- coda.samples(jagsModel, variable.names=monitor, n.iter=100000, thin=1)

#time.adapt <- system.time(jagsModel <- jags.model("model.bugs", data=dataList, n.chains = 1, n.adapt = 200000))
#show(time.adapt)

#update(jagsModel, n.iter=1000)

#
#sum.saved <- summary(post)$statistics
#show(sum.saved)
#show(time.update)
#mcmcplot(post)
#pannaan talteen probsit, niin saadaan 3-ulotteinen taulukko todennäköisyydet



#intercepts <- GetLabelNamesForGgMcmc("b.lang.ref",list(levels(aineisto$lang),levels(aineisto$ref), levels(aineisto$loc)))
#intercepts <- GetLabelNamesForGgMcmc("lang.morph",list(levels(aineisto$lang),levels(aineisto$morph), levels(aineisto$loc)))
#intercepts$lang <- "fi"
#intercepts$lang[grepl("ru,",intercepts$Label)] <- "ru"
#intercepts$loc <- "S1"
#intercepts$loc[grepl("S2",intercepts$Label)] <- "S2"
#intercepts$loc[grepl("S3",intercepts$Label)] <- "S3"
#intercepts$loc[grepl("S4",intercepts$Label)] <- "S4"

#

intercepts <- GetLabelNamesForGgMcmc("lang.subjtype",list(levels(aineisto$lang), levels(aineisto$subjtype), levels(aineisto$loc)))
p<-ggs(post, family="^lang.subjtype", par_labels=intercepts)

subjtype.names <- GetLabelNamesForGgMcmc("subjtype",list(levels(aineisto$subjtype), levels(aineisto$loc)))
p.subjtype<-ggs(post, family="^subjtype", par_labels=subjtype.names)
ggs_caterpillar(p, family="fi") 
ggs_caterpillar(p.subjtype) 

#ggs_caterpillar(ggs(post, par_labels=intercepts),family="ADV") + aes(color=lang) + facet_wrap(~ loc)

#saveRDS(post,"../phdmanuscript/monograph/data/dumps/hierarchical_dirichlecht_morph-ref-funct-clausestatus-subjtype-objtype.rds")


