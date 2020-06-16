## Title: EvapModule_AE
## 
## Interface/Abstraction: This object follows the "modules" 
## interface consisting of the methods;SetUp(), Execute(), Update()
##
## Description: This class takes a given evaporation value and pulls
## that value from the soil regardless of wilting point. If the top 
## layer can not satisfy the entire evaporative demand, the water 
## from the next layer is pulled up. This is the first dev version
## of an evaporative module that sucks the layer to 0 (use PE w/ wp)
## 
## Inputs: soilModData (R6 class - args: soilProfile, t_dat,zDat)
## Methods: SetUp, Execute, Update
##
##@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@     
# TranspModule_AT Class Generator ---------------------------
EvapModule_AE <- R6Class(
  classname="EvapModule_AE",
  public = list(
    soilModData = NULL, 
    
    initialize = function(soilModData, module_item = NULL) {
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
        !is.null(self$soilModData$t_dat$PE)
      )
      self$soilModData$t_dat$AE <- 0
      self$soilModData$z_dat$AE <- 0
    },
    
    execute = function(t) {
      for (i in 1:length(self$soilModData$soilProfile$soil_layers)) {
        if (self$soilModData$t_dat$PE[t] != 0) {
          self$soilModData$soilProfile$soil_layers[[i]] <- 
            private$.transpCalcFun(self$soilModData$soilProfile$soil_layers[[i]], 
                                   self$soilModData$t_dat$PT[t])
        } else {
          self$soilModData$soilProfile$soil_layers[[i]]$AE <- 0
        }
      }
      # take water out of top layer
      
      
      # if not enough water take from next layer
      
      
      
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
      # update AE in t_dat
      
      
      self$soilModData$t_dat$AT[t] <- 
        rbindlist(self$soilModData$soilProfile$soil_layers)$AT %>% 
        sum()
      ifelse(
        self$soilModData$t_dat$root_frac[t] < 1,
        {self$soilModData$t_dat$AT_sub_soil[t] <- 
          self$soilModData$t_dat$PT[t] - self$soilModData$t_dat$AT[t]},
        {self$soilModData$t_dat$AT_sub_soil[t] <- 0}
      )
    }
  ),
  
  private = list(
    .evapCalc = function(soil_layer, PE) {
      soil_layer$AE <- PT * soil_layer$root_frac
      soil_layer$vwc <- soil_layer$vwc - soil_layer$AT / soil_layer$thickness
      return(soil_layer)
    }
  )
)


