## Title: TranspModule_AET
## 
## Interface/Abstraction: This object follows the "modules" 
## interface consisting of the methods;SetUp(), Execute(), Update()
##
## Description: This class models uniform transpiration across the root depth.
## The user must supply the transpiration rates in the 't' level data frame
## that will be uniformly distributed across the root profile.
## 
## Inputs: soilModData (R6 class - args: soilProfile, t_dat,zDat)
## Methods: SetUp, Execute, Update
##
##@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@     
# TranspModule_AET Class Generator ---------------------------
TranspModule_AET <- R6Class(
  classname="TranspModule_AET",
  public = list(
    soilModData = NULL, 

    initialize = function(soilModData, mod_data_loc = NULL) {
      stopifnot(
        exists("t_dat", soilModData),
        exists("z_dat", soilModData),
        exists("io_path", soilModData),
        !is.null(soilModData$z_dat$vwc),
        is.numeric(soilModData$z_dat$vwc), 
        all(soilModData$z_dat$vwc > 0 & soilModData$z_dat$vwc < 1)
      )
      self$soilModData <- soilModData
    },
    
    setUp = function() {
      stopifnot(
        !is.null(self$soilModData$t_dat$root_depth),
        !is.null(self$soilModData$t_dat$PT)
      )
      self$soilModData$t_dat$AT <- 0
      self$soilModData$z_dat$AT <- 0
    },
    
    execute = function(t) {
      for (i in 1:length(self$soilModData$soilProfile$soil_layers)) {
        if (self$soilModData$t_dat$PT[t] != 0) {
          self$soilModData$soilProfile$soil_layers[[i]] <- 
            private$.transpCalcFun(self$soilModData$soilProfile$soil_layers[[i]], 
                                   self$soilModData$t_dat$PT[t])
        } else {
          self$soilModData$soilProfile$soil_layers[[i]]$AT <- 0
        }
      }
    },
    
    update = function(t) {
      self$soilModData$t_dat$AT[t] <- 
        rbindlist(self$soilModData$soilProfile$soil_layers)$AT %>% 
        sum()
    }
  ),
  
  private = list(
    .transpCalcFun = function(soil_layer, PT) {
      soil_layer$AT <- PT * soil_layer$root_frac
      soil_layer$vwc <- soil_layer$vwc - soil_layer$AT / soil_layer$thiccness
      return(soil_layer)
    }
  )
)


