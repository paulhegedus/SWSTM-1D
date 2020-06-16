## Title: EvapModule_PE
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
# EvapModule_PE Class Generator ---------------------------
EvapModule_PE <- R6Class(
  classname="EvapModule_PE",
  public = list(
    soilModData = NULL, 
    mod_data_loc = NULL,
    
    initialize = function(soilModData, module_item) {
      stopifnot(
        exists("t_dat", soilModData),
        exists("z_dat", soilModData),
        exists("io_path", soilModData),
        !is.null(soilModData$z_dat$vwc),
        is.numeric(soilModData$z_dat$vwc), 
        all(soilModData$z_dat$vwc > 0 & soilModData$z_dat$vwc < 1)
      )
      if (grepl("z_dat", names(module_item))) {
        stopifnot(
          file.exists(paste0(soilModData$io_path, "/inputs/", 
                             module_item$z_dat,".csv"))
        )
        self$mod_data_loc <- module_item$z_dat
      } 
      self$soilModData <- soilModData
    },
    
    setUp = function() {
      if (!is.null(self$mod_data_loc)) {
        dat_in <- fread(paste0(self$soilModData$io_path, 
                               "/inputs/", 
                               self$mod_data_loc,".csv")) %>%
          as.data.frame()
        stopifnot(
          is.data.frame(dat_in),
          !is.null(dat_in$wp),
          is.numeric(dat_in$wp),
          nrow(dat_in) == nrow(self$soilModData$z_dat)
        )
        self$soilModData$z_dat$wp <- dat_in$wp
      }
      stopifnot(
        !is.null(self$soilModData$z_dat$wp)
        !is.null(self$soilModData$t_dat$root_depth),
        !is.null(self$soilModData$t_dat$PE)
      )
      
      # 2) Input data has to be modified
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
      if (soil_layer$vwc - soil_layer$wp > evap_budget / soil_layer$thiccness) {
        soil_layer$AE <- evap_budget
        soil_layer$vwc <- soil_layer$vwc - soil_layer$AE / soil_layer$thiccness
      } else {
        soil_layer$AE <- soil_layer$vwc - soil_layer$wp * soil_layer$thiccness
        soil_layer$vwc <- soil_layer$wp
      }
      return(soil_layer)
    }
  )
)


