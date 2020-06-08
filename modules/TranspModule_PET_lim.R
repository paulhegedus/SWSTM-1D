## Title: TranspModule_Uniform
## 
## Interface/Abstraction: This object follows the "modules" 
## interface consisting of the methods;SetUp(), Execute(), Update(), plotGen()
##
## Description: This class models uniform transpiration across the root depth.
## The user must supply the wilting point for every depth interval so that 
## transpiration can be calculated by the uniform uptake of PAW.
## 
## Inputs: soilModData (R6 class - args: soilProfile, tDat,zDat)
## Methods: SetUp, Execute, Update
##
##@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@     
# DrainModuleFC Class Generator ---------------------------
TranspModule_Uniform <- R6Class(
  classname="TranspModule_Uniform",
  public = list(
    soilModData = NULL, 
    modDataLoc = NULL,
    
    initialize = function(soilModData, modDataLoc) {
      stopifnot(
        exists("tDat", soilModData),
        exists("zDat", soilModData),
        exists("ioPath", soilModData),
        file.exists(paste0(soilModData$ioPath, 
                           "/inputs/", 
                           modDataLoc,".csv")),
        !is.null(soilModData$zDat$vwc),
        is.numeric(soilModData$zDat$vwc), 
        all(soilModData$zDat$vwc > 0 & soilModData$zDat$vwc < 1)
      ) 
      self$soilModData <- soilModData
      self$modDataLoc <- modDataLoc
    },
    
    SetUp = function() {
      # 1) Modules specific data must be in folder named 'inputs'
      dfcIn <- fread(paste0(self$soilModData$ioPath, 
                            "/inputs/", 
                            self$modDataLoc,".csv")) %>%
        as.data.frame()
      stopifnot(
        is.data.frame(dfcIn),
        !is.null(dfcIn$wp),
        is.numeric(dfcIn$wp),
        nrow(dfcIn) == nrow(self$soilModData$zDat),
        !is.null(self$soilModData$tDat$rootDepth)
      )
      # 2) Input data has to be modified
      self$soilModData$zDat$wp <- dfcIn$wp
      self$soilModData$zDat$transp <- 0
      # 3) Output data has to be modified (0 added as defaults to avoid elses)
      self$soilModData$tDat$transp <- 0 
    },
    
    Execute = function(t) {
      for (i in 1:length(self$soilModData$soilProfile$soilLayers)) {
        if (self$soilModData$tDat$rootDepth[t] != 0) {
          self$soilModData$soilProfile$soilLayers[[i]] <- 
            private$.TranspCalcFun(self$soilModData$soilProfile$soilLayers[[i]])
        } else {
          self$soilModData$soilProfile$soilLayers[[i]]$transp <- 0
        }
      }
    },
    
    Update = function(t) {
      self$soilModData$tDat$transp[t] <- 
        rbindlist(self$soilModData$soilProfile$soilLayers)$transp %>% 
        sum()
    }
  ),
  
  private = list(
    .TranspCalcFun = function(soilLayer) {
      if (soilLayer$vwc > soilLayer$wp) {
        paw <- soilLayer$vwc - soilLayer$wp
        soilLayer$transp <- paw * soilLayer$rootDepth
        soilLayer$vwc <- soilLayer$vwc - soilLayer$transp / soilLayer$depth
      }
      return(soilLayer)
    }
  )
)


