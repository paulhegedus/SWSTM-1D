## Title: DrainModuleFC
## 
## Interface/Abstraction: This object follows the "modules" 
## interface consisting of the methods;SetUp(), Execute(), Update(), plotGen()
##
## Description: This class is the instantaneous drain module for the 1d soil 
## simulation model. The exe method determines if precip available, and then 
## for every layer in the class, the function calculates how much water goes 
## in or out of each layer.
##
## Inputs: soilModData (R6 class - args: soilProfile, tDat,zDat)
## Methods: SetUp, Execute, Update, plotGen
##
##@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@     
# DrainModuleFC Class Generator ---------------------------
DrainModuleFC <- R6Class(
  classname="DrainModuleFC",
  public = list(
    soilModData = NULL, 
    mod_data_loc = NULL,

    initialize = function(soilModData, module_item) {
      stopifnot(
        exists("t_dat", soilModData),
        exists("z_dat", soilModData),
        exists("io_path", soilModData),
        file.exists(paste0(soilModData$io_path, 
                           "/inputs/", 
                           module_item$z_dat,".csv")),
        !is.null(soilModData$z_dat$vwc),
        is.numeric(soilModData$z_dat$vwc), 
        all(soilModData$z_dat$vwc > 0 & soilModData$z_dat$vwc < 1)
      ) 
      self$soilModData <- soilModData
      self$mod_data_loc <- module_item$z_dat
    },
    
    setUp = function() {
      # 1) Modules specific data must be in folder named 'inputs'
      dat_in <- fread(paste0(self$soilModData$io_path, 
                            "/inputs/", 
                            self$mod_data_loc,".csv")) %>%
        as.data.frame()
      stopifnot(
        is.data.frame(dat_in),
        !is.null(dat_in$fc),
        is.numeric(dat_in$fc),
        nrow(dat_in) == nrow(self$soilModData$z_dat) 
      )
      # 2) Input data has to be modified
      self$soilModData$z_dat$fc <- dat_in$fc
      # 3) Output data has to be modified (0 added as defaults to avoid elses)
      self$soilModData$t_dat$deep_perc <- 0 
      self$soilModData$z_dat$w_top <- 0 
      self$soilModData$z_dat$w_bot <- 0 
    },
    
    execute = function(t) {
      # Get the number of soil layers for shorter pointer
      num_layers <- length(self$soilModData$soilProfile$soil_layers)
      if (!is.null(self$soilModData$t_dat$prec)) { 
        # Checked if the user added a precip column. If not null, use precip.
        self$soilModData$soilProfile$soil_layers[[1]]$w_top <- 
          self$soilModData$t_dat$prec[t] / 
          self$soilModData$soilProfile$soil_layers[[1]]$thickness
      } # Else not needed b/c default set to 0
      for (i in 1:num_layers) {
        self$soilModData$soilProfile$soil_layers[[i]] <- 
          private$.drainFunFC(self$soilModData$soilProfile$soil_layers[[i]])
        if (i != num_layers) {
          self$soilModData$soilProfile$soil_layers[[i+1]]$w_top <- 
            self$soilModData$soilProfile$soil_layers[[i]]$w_bot
        }
      }
    },
    
    update = function(t) {
      # Get the number of soil layers for shorter pointer
      num_layers <- length(self$soilModData$soilProfile$soil_layers)
      self$soilModData$t_dat$deep_perc[t] <- 
        self$soilModData$soilProfile$soil_layers[[num_layers]]$w_bot * 
        self$soilModData$soilProfile$soil_layers[[num_layers]]$thickness
    }
  ),
  
  private = list(
    .drainFunFC = function(soil_layer) {
      soil_layer$vwc <- soil_layer$vwc + soil_layer$w_top
      if (soil_layer$vwc > soil_layer$fc) {
        soil_layer$w_bot <- (soil_layer$vwc - soil_layer$fc) * soil_layer$thickness
        soil_layer$vwc <- soil_layer$vwc - soil_layer$w_bot
      } # Else not needed b/c default set to 0 
      return(soil_layer)
    }
  )
)


