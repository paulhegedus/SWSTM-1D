## Title: TranspModule_PT_noLim
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
# TranspModule_PET_noLim Class Generator ---------------------------
TranspModule_PT_noLim <- R6Class(
  classname="TranspModule_PT_noLim",
  public = list(
    soilModData = NULL, 
    mod_dat_name = NULL,
    
    initialize = function(soilModData, module_item) {
      stopifnot(
        exists("t_dat", soilModData),
        exists("z_dat", soilModData),
        exists("io_path", soilModData),
        !is.null(soilModData$z_dat$vwc),
        is.numeric(soilModData$z_dat$vwc), 
        all(soilModData$z_dat$vwc > 0 & soilModData$z_dat$vwc < 1),
        file.exists(paste0(soilModData$io_path, "/inputs/", 
                           module_item$z_dat,".csv"))
        #any(grepl("z_dat", names(module_item)))
      )
      self$mod_dat_name <- module_item$z_dat
      self$soilModData <- soilModData
    },
    
    setUp = function() {
      if (!is.null(self$mod_dat_name)) {
        dat_in <- fread(paste0(self$soilModData$io_path, 
                               "/inputs/", 
                               self$mod_dat_name,".csv")) %>%
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
        !is.null(self$soilModData$z_dat$wp),
        !is.null(self$soilModData$t_dat$root_depth),
        !is.null(self$soilModData$t_dat$PT)
      )
      
      # 2) Input data has to be modified
      self$soilModData$t_dat$AT_soil_zone <- 0
      self$soilModData$z_dat$AT_soil_zone <- 0
      self$soilModData$t_dat$AT_sub_soil <- 0
    },
    
    execute = function(t) {
      for (i in 1:length(self$soilModData$soilProfile$soil_layers)) {
        if (self$soilModData$t_dat$PT[t] != 0) {
          self$soilModData$soilProfile$soil_layers[[i]] <- 
            private$.transpCalcFun(self$soilModData$soilProfile$soil_layers[[i]], 
                                   self$soilModData$t_dat$PT[t])
        } else {
          self$soilModData$soilProfile$soil_layers[[i]]$AT_soil_zone <- 0
        }
      }
    },
    
    update = function(t) {
      self$soilModData$t_dat$AT_soil_zone[t] <- 
        rbindlist(self$soilModData$soilProfile$soil_layers)$AT_soil_zone %>% 
        sum()
    }
  ),
  
  private = list(
    .transpCalcFun = function(soil_layer, PT) {
      AT <- PT * soil_layer$root_frac
      new_vwc <- soil_layer$vwc - AT / soil_layer$thickness
      if (new_vwc >= soil_layer$wp) {
        soil_layer$AT_soil_zone <- AT
        soil_layer$vwc <- new_vwc
      } else {
        soil_layer$AT_soil_zone <- soil_layer$vwc - soil_layer$wp * soil_layer$thickness
        soil_layer$vwc <- soil_layer$wp
      }
      return(soil_layer)
    }
  )
)


