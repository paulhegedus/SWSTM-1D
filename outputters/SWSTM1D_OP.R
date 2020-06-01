## Title: SWSTM1D_OP Outputter
## 
## Interface/Abstraction: This object follows the "outputters" 
## interface consisting of the methods; write_z and write_t
## write_z: write depth (z) level data at each timestep (t)
## write_t: writes t level data at each timestep (t)
##
## Description: This class is the general outputter for the SWSTM-1D model.
## The Outputter class generator makes a outputter object that saves
## data. Plot output will be generated later.

## Inputs: soilModData
## Methods: write_z, write_t
##
##@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@
# File OutPutter Class Generator ---------------------------
## Outputter that saves data from swstm1d simulation
SWSTM1D_OP <- R6Class(
  "SWSTM1D_OP",
  public = list(
    soilModData = NULL,
    
    initialize = function(soilModData) {
      stopifnot(
        exists("tDat", soilModData),
        exists("zDat", soilModData),
        exists("ioPath", soilModData)
      ) 
      self$soilModData <- soilModData
      # TODO:
      # open file connections
      # write headers for files
    },
    write_z = function(t) {
      # TODO: write to open file connection & write all rows of z dat for time t
      zDat_append <- do.call(rbind.data.frame,
                             lapply(self$soilModData$soilProfile$soilLayers,
                                    as.data.frame))
      zDat_append$time <- t
      fwrite(zDat_append, 
             paste0(self$soilModData$ioPath, "/outputs/zOut/ZxT/zDat_t", t, ".csv"))
    },
    write_t = function() {
      # TODO: write to open file connection and write t row of t dat
      fwrite(self$soilModData$tDat,
             paste0(self$soilModData$ioPath, "/outputs/tOut/tDat_T.csv"))
    }
  )
  #private = list()
)







