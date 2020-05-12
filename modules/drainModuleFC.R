## Title: DrainModuleFC
## 
## Interface/Abstraction: This object follows the "modules" interface consisting of the methods; 
## setUp(), execute(), update(), plotGen()
##
## Description: This class is the instantaneous drain module for the 1d soil simulation model.
## The exe method determines if precip available, and then for every layer in the 
## class, the function calculates how much water goes in or out of each layer.
## 
## Inputs: soilModData (R6 class - args: soilProfile, tDat,zDat)
## Methods: setup, execute, update, plotGen
##
##@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@
#### Packages ####
##@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@
## Packages specific to this module
# < none >

##@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@
#### Class  ####
##@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@
DrainModuleFC <- R6Class(
  classname="DrainModuleFC",
  public = list(
    soilModData = NULL, 
    #t = NULL, # have to know the timestep
    
    initialize = function(soilModData){
      stopifnot(exists("tDat",soilModData),
                exists("zDat",soilModData),
                exists("soilProfile",soilModData),
                is.data.frame(soilModData$tDat), # redundant to initializer in SoilModData?
                is.data.frame(soilModData$zDat), # redundant to initializer in SoilModData?
                any(grepl("time",names(soilModData$tDat))), # redundant to initializer in SoilModData?
                any(grepl("fc",names(soilModData$zDat))),
                any(grepl("vwc",names(soilModData$zDat))), 
                any(grepl("depth",names(soilModData$zDat))), # redundant to initializer in SoilModData?
                is.numeric(soilModData$tDat$time), # redundant to initializer in SoilModData?
                is.numeric(soilModData$zDat$depth), # redundant to initializer in SoilModData?
                is.numeric(soilModData$zDat$time), # redundant to initializer in SoilModData?
                is.numeric(soilModData$zDat$fc),
                is.numeric(soilModData$zDat$vwc), 
                all(soilModData$zDat$depth>0), # redundant to initializer in SoilModData?
                all(soilModData$zDat$vwc>0 & soilModData$zDat$vwc<1),
                exists("soilLayers",soilModData$soilProfile),
                is.numeric(t)) #
      self$soilModData <- soilModData
      self$t <- t
    },
    setup = function(){
      ##**** TODO 
      ## make a setter function for adding columns
      ## make it generala... addCol <- function(df,newColName,default)
      
      # would have to 
      if(any(grepl("prec",names(self$soilModData$tDat)))){
        self$soilModData$soilProfile$soilLayers[[1]]$wTop <- self$soilModData$tDat$prec[self$t]
      }else{
        
        self$soilModData$soilProfile$soilLayers[[1]]$wTop <- 0
      }
      return(invisible(self))
    },
    execute = function(){
      for(i in 1:length(self$soilModData$soilProfile$soilLayers)){
        self$soilModData$soilProfile$soilLayers[[i]] <- private$drainFCfun(self$soilModData$soilProfile$soilLayers[[i]])
        if(i!=length(self$soilModData$soilProfile$soilLayers)){
          self$soilModData$soilProfile$soilLayers[[i+1]]$wTop <- self$soilModData$soilProfile$soilLayers[[i]]$wBot
        }
      }
      return(invisible(self))
      
    },
    update = function(){
      ## update the t level data
      self$soilModData$tDat$deepPerc[self$t] <- self$soilModData$soilProfile$soilLayers[[length(self$soilModData$soilProfile$soilLayers)]]$wBot
      
      ## update the z level data
      zDat_append <- do.call(rbind.data.frame,
                             self$soilModData$soilProfile$soilLayers %>%
                               lapply(as.data.frame))
      zDat_append$time <- self$t
      self$soilModData$zDat <- bind_rows(self$soilModData$zDat,zDat_append)
      
      return(invisible(self))
    },
    plotGen = function(){
      # make plots (call private plot fxns)
      
    }
  ),
  private = list(
    drainFCfun = function(soilLayer){
      soilLayer$vwc <- soilLayer$vwc + soilLayer$wTop
      if(soilLayer$vwc > soilLayer$fc){
        soilLayer$wBot <- soilLayer$vwc - soilLayer$fc * soilLayer$depth
        soilLayer$vwc <- soilLayer$vwc - soilLayer$wBot
      }else{
        soilLayer$wBot <- 0
      }
      return(soilLayer)
    }
  )
)







