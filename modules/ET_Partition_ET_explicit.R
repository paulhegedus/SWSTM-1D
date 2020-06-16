## Title: ET_Partition_ET_explicit
## 
## Interface/Abstraction: This object follows the "modules" 
## interface consisting of the methods;SetUp(), Execute(), Update()
##
## Description: This class is part of the ET_Partition interface that 
## ET to E and T based on explicit inputs from the user. User has to pass
## in a E and T column.
## 
## Inputs: soilModData (R6 class - args: soilProfile, t_dat,zDat)
## Methods: SetUp, Execute, Update
##
##@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@     
# ET_Partition_ET_explicit Class Generator ---------------------------
ET_Partition_ET_explicit <- R6Class(
  classname="ET_Partition_ET_explicit",
  public = list(
    soilModData = NULL, 
    mod_data_loc = NULL,
    
    initialize = function(soilModData, module_item) {
      stopifnot(
        exists("t_dat", soilModData),
        exists("z_dat", soilModData),
        exists("io_path", soilModData),
        file.exists(paste0(soilModData$io_path, "/inputs/", 
                           module_item$t_dat,".csv"))
      )
      
      self$soilModData <- soilModData
      self$mod_data_loc <- module_item$t_dat
    },
    
    setUp = function() {
      # 1) Modules specific data must be in folder named 'inputs'
      dat_in <- fread(paste0(self$soilModData$io_path, 
                             "/inputs/", 
                             self$mod_data_loc,".csv")) %>%
        as.data.frame()
      stopifnot(
        is.data.frame(dat_in),
        nrow(dat_in) == nrow(self$soilModData$t_dat),
        !is.null(dat_in$E), # Must have E defined as column
        !is.null(dat_in$T) # Must have T defined as column      
      )
      # 2) Input data has to be modified
      self$soilModData$t_dat$PE <- dat_in$E
      self$soilModData$t_dat$PT <- dat_in$T
    },
    
    execute = function(t) {},
    
    update = function(t) {}
  )
)


