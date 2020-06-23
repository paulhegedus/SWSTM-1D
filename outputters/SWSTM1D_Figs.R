## Title: SWSTM1D_Figs Outputter
## 
## Interface/Abstraction: This object follows the "outputters" 
## interface consisting of the methods; Write_z and Write_t
## Write_z: write depth (z) level data at each timestep (t)
## Write_t: writes t level data at each timestep (t)
## runOutput: does something
## closeCon: closes any open connections
##
## Description: This class is the general figure making outputter 
## for the SWSTM-1D model. The Outputter class generator makes a 
## outputter object that saves data. Plot output will be generated later.

## Inputs: soilModData
## Methods: runOutput()
##
##@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@
# File OutPutter Class Generator ---------------------------
## Outputter that saves data from swstm1d simulation
SWSTM1D_Figs <- R6Class(
  "SWSTM1D_Figs",
  public = list(
    soilModData = NULL,
    z_con = NULL,
    t_con = NULL,
    op_ints = NULL,

    initialize = function(soilModData, op_list) {
      stopifnot(
        exists("io_path", soilModData)
      ) 
      self$soilModData <- soilModData
      self$op_ints <- op_list$op_ints
      
      # Write initial z and t level info & open connection
      self$z_con <- paste0(self$soilModData$io_path, "/outputs/z_dat.csv")
      self$t_con <- paste0(self$soilModData$io_path, "/outputs/t_dat.csv")
    },
    writeZ = function(t) {},
    writeT = function(t) {},
    runOutput = function() {
      ## MB I/O - Line
      
      
      ## MB Partition - Line x Color
      
      
      ## Precip x Time - Bar
      
      
      ## VWC x Time - Bar
      
      
    },
    closeCon = function() {}
  )
  #private = list()
)







