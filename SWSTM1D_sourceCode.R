## SWSTM1D - Soil Water and Solute Transport Model 1D
## Source Code (Non-Module Specific Classes)
##
## Paul Hegedus
## Date: 05/04/2020 

## Description:
## Classes and functions for the 1D water and solute transort model. 
## First loads packages and functions for swstm1d
## Then checks model space for folders called 'modules' and 'inputs'.
## Next, sources all of the R6 classes not specific to modules.

## Notes:
## 
##@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@
library(R6)
library(tidyverse)
library(DescTools)
library(data.table)

# Check Dependencies ---------------------------
CheckForModelReqs <- function(modPath, ioPath) {
  if (!file.exists(paste0(modPath, "/modules"))) {
    stop("Path to 'modules' empty.")
  }
  if (!file.exists(paste0(ioPath, "/inputs"))) {
    stop("Path to 'inputs' empty.")
  } else {
    if (!file.exists(paste0(ioPath, "/inputs/tIn_dat.csv"))) {
      stop("Time level inputs ('tIn_dat.csv') not found.")
    }
    if (!file.exists(paste0(ioPath, "/inputs/zIn_dat.csv"))) {
      stop("Depth (t=0) level inputs ('zIn_dat.csv') not found.")
    }
  }
}

# SWSTM1D Class Generator ---------------------------
# Soil Water and Solute Transport Model 1D
# 'modPath' and 'ioPath' are both needed so that the model and 
# inputs/outputs can be in different locations

SWSTM1D <- R6Class(
  "SWSTM1D", 
  public = list(
    soilModData = NULL,  
    soilModList = NULL,  
    opList = NULL,  
    modsDataLoc = NULL,

    initialize = function(modPath, ioPath, tInName, zInName, mods_select, op_select, mods_data_loc) {
      tDat <- fread(paste0(ioPath, "/inputs/", tInName, ".csv")) %>%
        as.data.frame()
      zDat <- fread(paste0(ioPath, "/inputs/", zInName, ".csv")) %>%
        as.data.frame()
      # 1) SoilModData class object has to be initialized first
      self$soilModData <- SoilModData$new(
        tDat = tDat,  
        zDat = zDat, 
        modPath = modPath,  
        ioPath = ioPath 
      )
      # 2) Lists for modules & outputters have to be generated from user input
      stopifnot(
        !is.null(mods_select)
        !is.null(op_select)
      )
      if (!is.null(mods_data_loc)) {
        stopifnot(
          length(mods_select) == length(mods_data_loc)
        )
      }
      if (!all(is.character(mods_select))) {
        mods_select <-  as.character(mods_select) 
      }
      if (!all(is.character(op_select))) {
        op_select <-  as.character(op_select) 
      }
      if (!all(is.character(mods_data_loc))) {
        mods_data_loc <- as.character(mods_data_loc) 
      }
      self$soilModList <- as.list(mods_select) %>% 
        `names<-`(mods_select) 
      self$modsDataLoc <- as.list(mods_data_loc) %>% 
        `names<-`(mods_data_loc) 
      self$opList <- as.list(op_select) %>% 
        `names<-`(op_select) 
    }, 
    SetUp = function() {
      # 1) The 'outputs' folder has to be created based on initial user inputs
      private$.MakeOutputsFolder()
      # 2) Modules have to be loaded and initialized from the 'modules' folder
      self$soilModList <- mapply(
        private$.LoadModules,
        self$soilModList,
        self$modsDataLoc
      )
      # 3) SoilModData structures have to be updated based on loaded modules
      lapply(
        self$soilModList,  
        private$.SetUpModules 
      )
      # 4) Outputters loaded after modules built
      self$opList <- lapply(
        self$opList,  
        private$.LoadOutputters
      )
      # 5) SoilProfile made after the SoilModData object is modified 
      self$soilModData$BuildSoilProfile()
      # 6) The initial soil profile at t=0 needs to be saved
      lapply(self$opList,
             private$.RunZToutputters,
             0)
    }, 
    Execute = function() {
      for (t in 1:nrow(self$soilModData$tDat)) {
        lapply(self$soilModList,
               private$.RunModules,
               t)
        lapply(self$opList,
               private$.RunZToutputters,
               t)
        lapply(self$opList,
               private$.RunToutputters,
               t)
      }
    }, 
    Output = function() {}
  ), 
  
  private = list(
    .MakeOutputsFolder = function() {
      owd <- paste0(self$soilModData$ioPath, "/outputs") 
      if (!file.exists(owd)) { 
        dir.create(owd)
        dir.create(paste0(owd, "/tOut"))
        dir.create(paste0(owd, "/zOut"))
      } else {
        if (!file.exists(paste0(owd, "/tOut"))) { 
          dir.create(paste0(owd, "/tOut"))
        }
        if (!file.exists(paste0(owd, "/zOut"))) { 
          dir.create(paste0(owd, "/zOut"))
        }
      }
    },
    .LoadModules = function(module, modDataLoc) {
      # 1) Have to source file
      source(paste0(self$soilModData$modPath, "/modules/", module, ".R"))
      # 2) Have to initialize module based on SoilModData
      module <- eval(parse(text = paste0(module, 
                                         "$new(self$soilModData, ", 
                                         modDataLoc,")")))
      return(module)
    },  
    .LoadOutputters = function(outputter) {
      # 1) Have to source file
      source(paste0(self$soilModData$modPath, "/outputters/", outputter, ".R"))
      # 2) Have to initialize outputter based on SoilModData
      outputter <- eval(parse(text = paste0(outputter, "$new(self$soilModData)")))
      return(outputter)
    },
    .SetUpModules = function(module) {
      module$SetUp() # module specific setup
    }, 
    .RunModules = function(module, t) { 
      module$Execute(t) 
      module$Update(t) 
    },
    .RunZToutputters = function(module_op, t) { 
      module_op$write_z(t) 
    },
    .RunToutputters = function(module_op, t) { 
      module_op$write_t(t) 
    }
  )
)

# SoilModData Class Generator ---------------------------
# Class of object that SWSTM1D relies on for functioning. Holds the time level 
# data and the current timestep depth level data and soil profile status. Also
# holds the path to the model scripts and inputs/outputs folder

SoilModData <- R6Class(
  classname = "SoilModData", 
  public = list(
    tDat = NULL,  
    zDat = NULL,   
    soilProfile = NULL, 
    modPath = NULL,  
    ioPath = NULL,  
    
    initialize = function(tDat, zDat, modPath, ioPath) {
      stopifnot(
        is.character(modPath), 
        is.character(ioPath), 
        is.data.frame(tDat),  
        is.data.frame(zDat),  
        any(grepl("time", names(tDat))), 
        any(grepl("time", names(zDat))),  
        any(grepl("depth", names(zDat))),  
        is.numeric(tDat$time), 
        is.numeric(zDat$time),  
        is.numeric(zDat$depth),  
        all(zDat$depth > 0),  
        length(unique(zDat$time)) == 1,  
        unique(zDat$time) == 0 # Checks that user knows what they're inputting
      ) 
      self$tDat <- tDat
      self$zDat <- zDat
      self$modPath <- modPath
      self$ioPath <- ioPath
      
      self$zDat$z <- private$.SumPrevNumFun(self$zDat$depth)
    }, 
    
    BuildSoilProfile = function() {
      self$soilProfile <- SoilProfile$new(self$zDat)
    }
  ), 
  
  private = list(
    .SumPrevNumFun = function(vec) {
      stopifnot(
        is.numeric(vec), 
        length(vec) > 1
      ) 
      for (i in 2:length(vec)) { 
        vec[i] <- vec[i] + vec[i-1]
      }
      return(vec)
    }
  )
)

# SoilProfile Class Generator ---------------------------
SoilProfile <- R6Class(
  "SoilProfile", 
  public = list( 
    soilLayers = NULL, 
    
    initialize = function(zDat) { 
      zDat$time <- NULL
      self$soilLayers <- apply(zDat, 1, as.list)
    }
  ) 
) 
















