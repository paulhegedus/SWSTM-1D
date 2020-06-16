## Title: TranspModule_AT
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
# TranspModule_AT Class Generator ---------------------------
TranspModule_AT <- R6Class(
  classname="TranspModule_AT",
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
        !is.null(self$soilModData$t_dat$root_depth),
        !is.null(self$soilModData$t_dat$PT)
      )
      self$soilModData$t_dat$AT <- 0
      self$soilModData$z_dat$AT <- 0
      
      ifelse(
        any(self$soilModData$t_dat$root_depth > max(self$soilModData$z_dat$z)),
        {self$soilModData$t_dat$AT_sub_soil <- 0},
        invisible()
      )
      
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
       ifelse(
         self$soilModData$t_dat$root_frac[t] < 1,
         {self$soilModData$t_dat$AT_sub_soil[t] <- 
           self$soilModData$t_dat$PT[t] - self$soilModData$t_dat$AT[t]},
         {self$soilModData$t_dat$AT_sub_soil[t] <- 0}
       )
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


