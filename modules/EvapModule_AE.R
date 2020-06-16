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
# TranspModule_AE Class Generator ---------------------------
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
      if (self$soilModData$t_dat$PE[t] != 0) {
        evap_budget <- self$soilModData$t_dat$PE[t]
        for (i in 1:length(self$soilModData$soilProfile$soil_layers)) {
          if (evap_budget > 0) {
            self$soilModData$soilProfile$soil_layers[[i]] <- 
              private$.evapCalcFun(self$soilModData$soilProfile$soil_layers[[i]],
                                   evap_budget)
            evap_budget <- 
              evap_budget - self$soilModData$soilProfile$soil_layers[[i]]$AE
          } else {
            break
          }
        }
      } else {
        lapply(self$soilModData$soilProfile$soil_layers, 
               function(soil_layer) soil_layer$AE <- 0)
      }
    },
    
    update = function(t) {
       self$soilModData$t_dat$AE[t] <- 
         rbindlist(self$soilModData$soilProfile$soil_layers)$AE %>% 
         sum()
    }
  ),
  
  private = list(
    .evapCalcFun = function(soil_layer, evap_budget) {
      if (soil_layer$vwc > evap_budget / soil_layer$thickness) {
        soil_layer$AE <- evap_budget
        soil_layer$vwc <- soil_layer$vwc - soil_layer$AE / soil_layer$thickness
      } else {
        soil_layer$AE <- soil_layer$vwc * soil_layer$thickness
        soil_layer$vwc <- 0
      }
      return(soil_layer)
    }
  )
)


