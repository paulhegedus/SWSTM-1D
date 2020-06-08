## Title: RootModule_UserSupplied
## 
## Interface/Abstraction: This object follows the "modules" 
## interface consisting of the methods;SetUp(), Execute(), Update()
##
## Description: This class is a root module where the user supplies the depth
## of roots at each time step. In this module, the root depths are distributed
## across each layer. Fractional root depths is permissable (i.e. 63.4 units 
## etc.). The 64th layer of the soilProfile has a rootDepth of 0.4 units.
##
## Inputs: soilModData (R6 class - args: soilProfile, tDat,zDat)
## Methods: SetUp, Execute, Update, plotGen
##
##@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@     
# DrainModuleFC Class Generator ---------------------------
RootModule_UserSupplied <- R6Class(
  classname="RootModule_UserSupplied",
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
                           modDataLoc,".csv"))
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
        !is.null(dfcIn$rootDepth),
        is.numeric(dfcIn$rootDepth),
        nrow(dfcIn) == nrow(self$soilModData$tDat),
        length(self$soilModData$soilProfile$soilLayers) < max(dfcIn$rootDepth) # FIX ME: change from length soilLayer to depth
      )
      # 2) Input data has to be modified
      self$soilModData$tDat$rootDepth <- dfcIn$rootDepth
      # 3) Output data has to be modified (0 added as defaults to avoid elses)
      self$soilModData$zDat$rootDepth <- 0 
    },
    
    Execute = function(t) {
      # Calculate the root depth at each soil layer for every time step
      root_depth <- self$soilModData$tDat$rootDepth[t]
      num_layers <- length(self$soilModData$soilProfile$soilLayers)
      # Find the layer where z > rootDepth, set root depth
      for (i in 1:num_layers) {
        if (root_depth != 0) {
          if (self$soilModData$soilProfile$soilLayers[[i]]$z >= root_depth) {
            non_root_depth <- 
              self$soilModData$soilProfile$soilLayers[[i]]$z - root_depth
            self$soilModData$soilProfile$soilLayers[[i]]$rootDepth <- 
              self$soilModData$soilProfile$soilLayers[[i]]$depth - non_root_depth
            break
          } else {
            self$soilModData$soilProfile$soilLayers[[i]]$rootDepth <- 
              self$soilModData$soilProfile$soilLayers[[i]]$depth
          }
        } else {
          self$soilModData$soilProfile$soilLayers[[i]]$rootDepth <- 0
        }
      }
    },
    
    Update = function(t) {}
  )#,
  #private = list()
)


