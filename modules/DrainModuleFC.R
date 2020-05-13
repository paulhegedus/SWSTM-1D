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
        exists("ioPath",soilModData),
                                    
        # check for input data in 'inputs' folder
        file.exists(paste0(soilModData$ioPath,"/inputs/DrainModuleFC_in.csv")),
        
        # column reqs in z dat
        !is.null(soilModData$zDat$vwc),
        is.numeric(soilModData$zDat$vwc), 
        all(soilModData$zDat$vwc>0 & soilModData$zDat$vwc<1)
      ) 
      self$soilModData <- soilModData
    },
    setup = function(){
      # 1) get input data
      dfcIn <- fread(
        paste0(
          self$soilModData$ioPath,
          "/inputs/DrainModuleFC_in.csv"
        )
      ) %>%
        as.data.frame()
      
      # check data
      stopifnot(
        is.data.frame(dfcIn),
        !is.null(dfcIn$fc),
        is.numeric(dfcIn$fc),
        nrow(dfcIn)==nrow(self$soilModData$zDat) # length of fc must be same as layers
      )
      
      ## 2) add input data to zDat
      self$soilModData$zDat$fc <- dfcIn$fc
      
      ## 3) add any output cols to output data
      ## only update tDat or zDat b/c soilProfile built after
      ## add columns to tDat
      self$soilModData$tDat$deepPerc <- 0 # make 0 as default
      ## add columns to zDat
      self$soilModData$zDat$wTop <- 0 # make 0 as default
      self$soilModData$zDat$wBot <- 0 # make 0 as default
    },
    execute = function(t){
      ## always do work on soil layers object
      num_layers <- length(self$soilModData$soilProfile$soilLayers)
      if(!is.null(self$soilModData$tDat$prec)){ 
        self$soilModData$soilProfile$soilLayers[[1]]$wTop <- 
          self$soilModData$tDat$prec[t]
      } # don't need else b/c default 0
      for(i in 1:num_layers){
        self$soilModData$soilProfile$soilLayers[[i]] <- 
          private$drainFCfun(self$soilModData$soilProfile$soilLayers[[i]])
        if(i!=num_layers){
          self$soilModData$soilProfile$soilLayers[[i+1]]$wTop <- 
            self$soilModData$soilProfile$soilLayers[[i]]$wBot
        }
      }
    },
    update = function(t){
      ## module updates the t level data
      num_layers <- length(self$soilModData$soilProfile$soilLayers)
      self$soilModData$tDat$deepPerc[t] <- 
        self$soilModData$soilProfile$soilLayers[[num_layers]]$wBot
      
      # make zDat for time step from soil layers
      zDat_append <- do.call(rbind.data.frame,
                             self$soilModData$soilProfile$soilLayers %>%
                               lapply(as.data.frame))
      zDat_append$time <- t
      
      # save in outputs folder
      self$soilModData$zDat <- bind_rows(self$soilModData$zDat,
                                         zDat_append)
    }
  ),
  private = list(
    drainFCfun = function(soilLayer){
      soilLayer$vwc <- soilLayer$vwc + soilLayer$wTop
      if(soilLayer$vwc > soilLayer$fc){
        soilLayer$wBot <- soilLayer$vwc - soilLayer$fc * soilLayer$depth
        soilLayer$vwc <- soilLayer$vwc - soilLayer$wBot
      } # don't need else b/c wBot set to 0 in setup()
      return(soilLayer)
    }
  )
)


##@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@
#### DrainModuleFC Outputter ####
##@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@


