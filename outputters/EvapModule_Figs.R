## Title: EvapModule_Figs Outputter
## 
## Interface/Abstraction: This object follows the "outputters" 
## interface consisting of the methods; Write_z and Write_t
## Write_z: write depth (z) level data at each timestep (t)
## Write_t: writes t level data at each timestep (t)
## runOutput: does something
## closeCon: closes any open connections
##
## Description: This class is for making figures related to the evaporation
## modules. This includes evaporation by time. 

## Inputs: soilModData
## Methods: runOutput()
##
##@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@
# File OutPutter Class Generator ---------------------------
## Outputter that saves data from swstm1d simulation
EvapModule_Figs <- R6Class(
  "EvapModule_Figs",
  public = list(
    soilModData = NULL,
    z_con = NULL,
    t_con = NULL,
    
    initialize = function(soilModData, op_list) {
      stopifnot(
        exists("io_path", soilModData)
      ) 
      self$soilModData <- soilModData
      
      # Write initial z and t level info & open connection
      self$z_con <- paste0(self$soilModData$io_path, "/outputs/z_dat.csv")
      self$t_con <- paste0(self$soilModData$io_path, "/outputs/t_dat.csv")
    },
    writeZ = function(t) {},
    writeT = function(t) {},
    runOutput = function() {
      ## Evap x Time - Bar
      
      
    },
    closeCon = function() {}
  )
  #private = list()
)







