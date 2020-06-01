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
    zCon = NULL,
    tCon = NULL,
    
    initialize = function(soilModData) {
      stopifnot(
        exists("tDat", soilModData),
        exists("zDat", soilModData),
        exists("ioPath", soilModData)
      ) 
      self$soilModData <- soilModData
      # Write initial z and t level info & open connection
      zDat <- do.call(
        rbind.data.frame,
        lapply(self$soilModData$soilProfile$soilLayers, as.data.frame)
      )
      zDat$time <- 0
      fwrite(zDat, paste0(self$soilModData$ioPath, "/outputs/zDat.csv"))
      
      tDat <- self$soilModData$tDat[1, ]
      tDat[1, ] <- NA
      tDat[1, "time"] <- 0
      fwrite(tDat, paste0(self$soilModData$ioPath, "/outputs/tDat.csv"))
      
      browser()
      self$zCon <- file(description = paste0(self$soilModData$ioPath, 
                                             "/outputs/zDat.csv"), 
                        open = "a")
      self$tCon <- file(description = paste0(self$soilModData$ioPath, 
                                             "/outputs/tDat.csv"), 
                        open = "a")
    },
    write_z = function(t) {
      # TODO: write to open file connection & write all rows of z dat for time t
      cat(rep(i, 7), file = zCon, append=TRUE, sep="\n")
      
    },
    write_t = function(t) {
      # TODO: write to open file connection and write t row of t dat
      cat(rep(i, 3), file = tCon, append=TRUE, sep="\n")
    },
    CloseConnection = function() {
      close(zCon)
      close(tCon)
    }
  )
  #private = list()
)







