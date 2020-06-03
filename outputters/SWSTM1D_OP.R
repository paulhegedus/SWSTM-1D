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
    ints = NULL,
    
    initialize = function(soilModData, ints) {
      stopifnot(
        exists("tDat", soilModData),
        exists("zDat", soilModData),
        exists("ioPath", soilModData),
        all(is.numeric(ints))
      ) 
      self$soilModData <- soilModData
      self$ints <- ints
      
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
      
      self$zCon <- file(description = paste0(self$soilModData$ioPath, 
                                             "/outputs/zDat.csv"),
                        open = "a")
      self$tCon <- file(description = paste0(self$soilModData$ioPath, 
                                             "/outputs/tDat.csv"),
                        open = "a")
    },
    write_z = function(t) {
      op <- ifelse(t > self$ints[1],
                   t / self$ints[1],
                   self$ints[1] / t)
      if (op == as.integer(op)) {
        zDat <- do.call(
          rbind.data.frame,
          lapply(self$soilModData$soilProfile$soilLayers, as.data.frame)
        )
        zDat$time <- t
        fwrite(zDat, file = self$zCon, append = TRUE) 
      }
    },
    write_t = function(t) {
      op <- ifelse(t > self$ints[2],
                   t / self$ints[2],
                   self$ints[2] / t)
      if (op == as.integer(op)) {
        tDat <- self$soilModData$tDat[t, ]
        fwrite(tDat, file = self$tCon, append = TRUE)  
      }
    }
  )
  #private = list()
)







