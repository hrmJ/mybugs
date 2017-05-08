#! /usr/bin/Rscript

GetLabelNamesForGgMcmc <- function(VarName, VarValues){
    # VarName= muuttuja
    # VarValues = lista, jossa arvot
    VarLengths <- lengths(VarValues)
    replist <- list()
    for(i in 1:length(VarValues)){
        repeach <- 1
        reptimes <- 1
        if(i==1){
            repeach <- 1
            reptimes <- prod(VarLengths[2:length(VarValues)])
        }
        else if(i<length(VarValues)){
            repeach <- prod(VarLengths[1:(i-1)])
            reptimes <- prod(VarLengths[(i+1):length(VarValues)])
        }
        else{
            repeach <- prod(VarLengths[1:(i-1)])
        }
        replist[[i]] <- list(reptimes=reptimes,repeach=repeach, repvals=c(1:length(VarValues[[i]])))
    }
    tobepasted <- list()
    idx <- 1
    for(thislist in replist){
        tobepasted[[idx]] <- rep(thislist$repvals,times=thislist$reptimes,each=thislist$repeach)
        idx <- idx +1
    }
    rawlabels <- do.call("paste", c(tobepasted, sep = ","))
    #show(tobepasted)
    tobepastednames <- list()
    idx <- 1
    for (thislist in tobepasted){
        thesenames <- c()
        for(val in thislist){
            thesenames <- c(thesenames, VarValues[[idx]][val])
        }
        tobepastednames[[idx]] <- thesenames
        idx <- idx +1
    }

    labels <- do.call("paste", c(tobepastednames, sep = ","))

    return (data.frame(Parameter=paste(VarName, "[", rawlabels, "]", sep=""), Label=labels))

}


intercepts <- GetLabelNamesForGgMcmc("b.lang.ref",list(levels(aineisto$lang),levels(aineisto$ref),levels(aineisto$morph), levels(aineisto$loc)))


