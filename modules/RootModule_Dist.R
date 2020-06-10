## Title: RootModule_Dist
## 
## Interface/Abstraction: This object follows the "modules" 
## interface consisting of the methods;SetUp(), Execute(), Update()
##
## Description: This class is a root module where the user supplies the depth
## of roots at each time step. In this module, the root mass is calculated
## for each layer. Fractional root depths is permissable (i.e. 63.4 units 
## etc.). The 64th layer of the soilProfile has a rootDepth of 0.4 units.
##
## Inputs: soilModData (R6 class - args: soilProfile, tDat,zDat)
## Methods: SetUp, Execute, Update, plotGen
##
## Reference:
# Fan, J., McConkey, B., Wang, H., & Janzen, H. (2016). 
#   Root distribution by depth for temperate agricultural crops. 
#   Field Crops Research, 189, 68–74. https://doi.org/10.1016/j.fcr.2016.02.013
##@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@     
# DrainModuleFC Class Generator ---------------------------
RootModule_Dist <- R6Class(
  classname = "RootModule_Dist",
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
        nrow(dfcIn) == nrow(self$soilModData$tDat)
      )
      # 2) Input data has to be modified
      self$soilModData$tDat$rootDepth <- dfcIn$rootDepth
      # 3) Output data has to be modified (0 added as defaults to avoid elses)
      self$soilModData$zDat$root <- 0 
      # FIXME: what 'root' is needs to be specified. Here it is root mass fraction
    },
    
    Execute = function(t) {
      # Calculate the root mass fraction at each soil layer for every time step
      root_depth <- self$soilModData$tDat$rootDepth[t]
      if (root_depth != 0) {
        layer_depths <- do.call(rbind.data.frame,
                                self$soilModData$soilProfile$soilLayers)$z
        root_mass <- private$.RootDistCalc(layer_depths, root_depth)
        root_mass[2:length(root_mass)] <- root_mass[2:length(root_mass)] - 
          root_mass[1:length(root_mass) - 1]
        
      } else {
        root_mass <- rep(0, length(self$soilModData$soilProfile$soilLayers))
      }
      for (i in 1:length(self$soilModData$soilProfile$soilLayers)) {
        self$soilModData$soilProfile$soilLayers[[i]]$root <- root_mass[i]
        # FIXME: what 'root' is needs to be specified. Here it is root mass fractio
      }
    },
    
    Update = function(t) {}
  ),
  private = list(
    .RootDistCalc = function(layer_depth, root_depth) {
      root_dist <- ifelse(layer_depth > root_depth,
                          1,
                          private$.FanCalc(layer_depth, root_depth))
      return(root_dist)
    },
    # FIXME: Hardcoded to wheat
    .FanCalc = function(d, dmax, da = 17.2, c = -1.286) {
      comp1 <- 1 / (1 + (d / da)^c)
      comp2 <- 1 - 1 / (1 + (dmax / da)^c) 
      comp3 <- d / dmax
      return(comp1 + comp2 * comp3)
    }
  )
)


