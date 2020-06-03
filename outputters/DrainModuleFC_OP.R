## Title: DrainModuleFC Outputter
## 
## Interface/Abstraction: This object follows the "outputters" 
## interface consisting of the methods; write_z and write_t
## write_z: write depth (z) level data at each timestep (t)
## write_t: writes t level data at each timestep (t)
##
## Description: This class is the instantaneous drain module outputter for the 1d soil 
## simulation model. This is related to the DrainModuleFC class
##
## Inputs: soilModData
## Methods: write_z, write_t
##
##@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@
# DrainModuleFC Outputter Class Generator ---------------------------
DrainModuleFC_OP <- R6Class(
  "DrainModuleFC_OP",
  public = list(
    soilModData = NULL,
    #zCon = NULL,
    #tCon = NULL,
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
      private$.MakeOutputsFolder()
      
      #self$zCon <- paste0(self$soilModData$ioPath, "/outputs/DrainModuleFC/   .csv")
      #self$tCon <- paste0(self$soilModData$ioPath, "/outputs/DrainModuleFC/   .csv")
    },
    # For saving module specific z data at each t step
    write_z = function(t) {
      op <- ifelse(t > self$ints[1],
                   t / self$ints[1],
                   self$ints[1] / t)
      if (op == as.integer(op)) { }
    },
    # For saving module specific t data after sim
    write_t = function(t) {
      op <- ifelse(t > self$ints[2],
                   t / self$ints[2],
                   self$ints[2] / t)
      if (op == as.integer(op)) { }
    }
  ),
  private = list(
    .MakeOutputsFolder = function() {
      owd <- paste0(self$soilModData$ioPath, "/outputs/DrainModuleFC") 
      if (!file.exists(owd)) { 
        dir.create(owd)
      }
    }
  )
)

















