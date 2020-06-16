## Title: RootModule_Length
## 
## Interface/Abstraction: This object follows the "modules" 
## interface consisting of the methods;SetUp(), Execute(), Update()
##
## Description: This class is a root module where the user supplies the depth
## of roots at each time step. In this module, the root lengths are distributed
## across each layer. Fractional root lengths are permissable (i.e. 63.4 units 
## etc.). The 64th layer of the soilProfile has a root_depth of 0.4 units. This
## module calculates root length at each depth, not mass.
##
## Inputs: soilModData (R6 class - args: soilProfile, t_dat,zDat)
## Methods: SetUp, Execute, Update, plotGen
##
##@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@     
# DrainModuleFC Class Generator ---------------------------
RootModule_Length <- R6Class(
  classname="RootModule_Length",
  public = list(
    soilModData = NULL, 
    mod_data_loc = NULL,
    
    initialize = function(soilModData, mod_data_loc) {
      stopifnot(
        exists("t_dat", soilModData),
        exists("z_dat", soilModData),
        exists("io_path", soilModData),
        file.exists(paste0(soilModData$io_path, 
                           "/inputs/", 
                           mod_data_loc,".csv"))
      ) 
      self$soilModData <- soilModData
      self$mod_data_loc <- mod_data_loc
    },
    
    setUp = function() {
      # 1) Modules specific data must be in folder named 'inputs'
      dat_in <- fread(paste0(self$soilModData$io_path, 
                            "/inputs/", 
                            self$mod_data_loc,".csv")) %>%
        as.data.frame()
      stopifnot(
        is.data.frame(dat_in),
        !is.null(dat_in$root_depth),
        is.numeric(dat_in$root_depth),
        nrow(dat_in) == nrow(self$soilModData$t_dat),
        max(self$soilModData$z_dat$z) >= max(dat_in$root_depth) 
      )
      # 2) Input data has to be modified
      self$soilModData$t_dat$root_depth <- dat_in$root_depth
      # 3) Output data has to be modified (0 added as defaults to avoid elses)
      self$soilModData$z_dat$root_frac <- 0 
    },
    
    execute = function(t) {
      # Calculate the root depth at each soil layer for every time step
      max_root_depth <- self$soilModData$t_dat$root_depth[t]
      num_layers <- length(self$soilModData$soilProfile$soil_layers)
      # Find the layer where z > max_root_depth, set root depth
      for (i in 1:num_layers) {
        if (max_root_depth != 0) {
          if (self$soilModData$soilProfile$soil_layers[[i]]$z >= max_root_depth) {
            non_root_depth <- 
              self$soilModData$soilProfile$soil_layers[[i]]$z - max_root_depth
            self$soilModData$soilProfile$soil_layers[[i]]$root_depth <- 
              self$soilModData$soilProfile$soil_layers[[i]]$thiccness - non_root_depth
            self$soilModData$soilProfile$soil_layers[[i]]$root_frac <- 
              self$soilModData$soilProfile$soil_layers[[i]]$root_depth / max_root_depth
            break
          } else {
            self$soilModData$soilProfile$soil_layers[[i]]$root_frac <- 
              self$soilModData$soilProfile$soil_layers[[i]]$root_depth / max_root_depth
          }
        } else {
          self$soilModData$soilProfile$soil_layers[[i]]$root_frac <- 0
        }
      }
    },
    
    update = function(t) {}
  )#,
  #private = list()
)


