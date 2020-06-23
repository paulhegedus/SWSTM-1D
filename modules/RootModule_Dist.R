## Title: RootModule_Dist
## 
## Interface/Abstraction: This object follows the "modules" 
## interface consisting of the methods;SetUp(), Execute(), Update()
##
## Description: This class is a root module where the user supplies the depth
## of roots at each time step. In this module, the root mass is calculated
## for each layer. Fractional root depths is permissable (i.e. 63.4 units 
## etc.). The 64th layer of the soilProfile has a root_depth of 0.4 units.
##
## Inputs: soilModData (R6 class - args: soilProfile, t_dat,zDat)
## Methods: SetUp, Execute, Update, plotGen
##
## References:
## Fan, J., McConkey, B., Wang, H., & Janzen, H. (2016). 
##   Root distribution by depth for temperate agricultural crops. 
##   Field Crops Research, 189, 68–74. https://doi.org/10.1016/j.fcr.2016.02.013
##@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@     
# DrainModuleFC Class Generator ---------------------------
RootModule_Dist <- R6Class(
  classname = "RootModule_Dist",
  public = list(
    soilModData = NULL, 
    mod_data_loc = NULL,
    fanEtAlMod = NULL,
    
    initialize = function(soilModData, module_item) {
      stopifnot(
        exists("t_dat", soilModData),
        exists("z_dat", soilModData),
        exists("io_path", soilModData),
        file.exists(paste0(soilModData$io_path, 
                           "/inputs/", 
                           module_item$t_dat,".csv"))
      ) 
      self$soilModData <- soilModData
      self$mod_data_loc <- module_item$t_dat
      self$fanEtAlMod <- FanEtAlMod$new(module_item$crop)
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
        nrow(dat_in) == nrow(self$soilModData$t_dat)
      )
      # 2) Input data has to be modified
      self$soilModData$t_dat$root_depth <- dat_in$root_depth
      # 3) Output data has to be modified (0 added as defaults to avoid elses)
      self$soilModData$t_dat$root_frac <- 0
      self$soilModData$z_dat$root_frac <- 0 
    },
    
    execute = function(t) {
     # browser()
      # Calculate the root mass fraction at each soil layer for every time step
      max_root_depth <- self$soilModData$t_dat$root_depth[t]
      if (max_root_depth != 0) {
        layer_depths <- do.call(rbind.data.frame,
                                self$soilModData$soilProfile$soil_layers)$z
        root_frac <- private$.rootDistCalc(layer_depths, max_root_depth)
        root_frac[2:length(root_frac)] <- root_frac[2:length(root_frac)] - 
          root_frac[1:length(root_frac) - 1]
        
      } else {
        root_frac <- rep(0, length(self$soilModData$soilProfile$soil_layers))
      }
      for (i in 1:length(self$soilModData$soilProfile$soil_layers)) {
        self$soilModData$soilProfile$soil_layers[[i]]$root_frac <- root_frac[i]
      }
    },
    
    update = function(t) {
      self$soilModData$t_dat$root_frac[t] <- 
        rbindlist(self$soilModData$soilProfile$soil_layers)$root_frac %>% 
        sum()
    }
  ),
  private = list(
    .rootDistCalc = function(layer_depth, max_root_depth) {
      root_frac <- ifelse(layer_depth > max_root_depth,
                          1,
                          self$fanEtAlMod$fanCalc(layer_depth, max_root_depth))
      return(root_frac)
    }
  )
)

FanEtAlMod <- R6Class(
  classname = "FanEtAlMod",
  public = list(
    crop = NULL,
    fan_table1 =  data.frame(
      crop = c("wheat"),
      da = c(17.2),
      c = c(-1.286)
    ),
    da = NULL,
    c = NULL,
    
    initialize = function(crop) {
      stopifnot(is.character(crop))
      self$crop <- crop
      self$da <- self$fan_table1[grep(crop,self$fan_table1$crop),"da"]
      self$c <- self$fan_table1[grep(crop,self$fan_table1$crop),"c"]
    },
    
    fanCalc = function(d, dmax) {
      comp1 <- 1 / (1 + (d / self$da)^self$c)
      comp2 <- 1 - 1 / (1 + (dmax / self$da)^self$c) 
      comp3 <- d / dmax
      return(comp1 + comp2 * comp3)
    }
  )
)







