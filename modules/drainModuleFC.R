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
    # arguments/fields
    soilModData = NULL, 

    # methods
    initialize = function(soilModData){
      stopifnot(
        # check if data structures exist
        exists("tDat",soilModData),
        exists("zDat",soilModData),
        exists("soilProfile",soilModData),

        # columns needed in z dat
        !is.null(soilModData$zDat$fc),
        !is.null(soilModData$zDat$vwc),
        is.numeric(soilModData$zDat$fc),
        is.numeric(soilModData$zDat$vwc), 
        
        # check vwc is b/w 0 and 1
        all(soilModData$zDat$vwc>0 & soilModData$zDat$vwc<1),
      ) 
      self$soilModData <- soilModData
    },
    setup = function(){
      ## only update tDat or zDat b/c soilProfile built after
      ## add columns to tDat
      self$soilModData$tDat$deepPerc <- 0 # make 0 as default
      ## add columns to zDat
      self$soilModData$zDat$wTop <- 0 # make 0 as default
      self$soilModData$zDat$wBot <- 0 # make 0 as default
    },
    execute = function(t){
      num_layers <- length(self$soilModData$soilProfile)
      if(!is.null(self$soilModData$tDat$prec)){ 
        self$soilModData$soilProfile[[1]]$wTop <- 
          self$soilModData$tDat$prec[t]
      } # don't need else b/c default 0
      for(i in 1:num_layers){
        self$soilModData$soilProfile[[i]] <- 
          private$drainFCfun(self$soilModData$soilProfile[[i]])
        if(i!=num_layers){
          self$soilModData$soilProfile[[i+1]]$wTop <- 
            self$soilModData$soilProfile[[i]]$wBot
        }
      }
    },
    update = function(t){
      ## update the t level data
      num_layers <- length(self$soilModData$soilProfile)
      self$soilModData$tDat$deepPerc[t] <- 
        self$soilModData$soilProfile[[num_layers]]$wBot
      
      ## update the z level data
      zDat_append <- do.call(rbind.data.frame,
                             self$soilModData$soilProfile %>%
                               lapply(as.data.frame))
      zDat_append$time <- t
      self$soilModData$zDat <- bind_rows(self$soilModData$zDat,
                                         zDat_append)
    },
    plotGen = function(){
      # make plots (call private plot fxns)
      # ?? should this be the job of an outputter ??
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
