## Title: TranspModule_AET
## 
## Interface/Abstraction: This object follows the "modules" 
## interface consisting of the methods;SetUp(), Execute(), Update(), plotGen()
##
## Description: This class models uniform transpiration across the root depth.
## The user must supply the transpiration rates in the 't' level data frame
## that will be uniformly distributed across the root profile.
## 
## Inputs: soilModData (R6 class - args: soilProfile, tDat,zDat)
## Methods: SetUp, Execute, Update
##
##@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@     
# DrainModuleFC Class Generator ---------------------------
TranspModule_AET <- R6Class(
  classname="TranspModule_AET",
  public = list(
    soilModData = NULL, 
    modDataLoc = NULL,
    
    initialize = function(soilModData, modDataLoc) {
      stopifnot(
        exists("tDat", soilModData),
        exists("zDat", soilModData),
        exists("ioPath", soilModData),
        !is.null(soilModData$zDat$vwc),
        is.numeric(soilModData$zDat$vwc), 
        all(soilModData$zDat$vwc > 0 & soilModData$zDat$vwc < 1),
        any(names(modDataLoc) == "ET") # Must have ET defined
      )
      for (i in 1:length(modDataLoc)) {
        stopifnot(
          file.exists(paste0(soilModData$ioPath, "/inputs/", modDataLoc[[i]],".csv"))
        )
      }
      self$soilModData <- soilModData
      self$modDataLoc <- modDataLoc
    },
    
    SetUp = function() {
      # 1) Modules specific data must be in folder named 'inputs'
      dfcIn <- fread(paste0(self$soilModData$ioPath, 
                            "/inputs/", 
                            self$modDataLoc$ET,".csv")) %>%
        as.data.frame()
      stopifnot(
        is.data.frame(dfcIn),
        nrow(dfcIn) == nrow(self$soilModData$tDat),
        !is.null(self$soilModData$tDat$rootDepth)
      )
      # 2) Input data has to be modified
      self$soilModData$tDat$transp <- dfcIn$transp
      self$soilModData$zDat$transp <- 0
    },
    
    Execute = function(t) {
      for (i in 1:length(self$soilModData$soilProfile$soilLayers)) {
        if (self$soilModData$tDat$transp[t] != 0) {
          self$soilModData$soilProfile$soilLayers[[i]] <- 
            private$.TranspCalcFun(self$soilModData$soilProfile$soilLayers[[i]], 
                                   self$soilModData$tDat$transp[t])
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
    .TranspCalcFun = function(soilLayer, transp) {
      soilLayer$transp <- transp * soilLayer$root
      # FIXME: what 'root' is needs to be specified
      soilLayer$vwc <- soilLayer$vwc - soilLayer$transp / soilLayer$depth
      return(soilLayer)
    }
  )
)


