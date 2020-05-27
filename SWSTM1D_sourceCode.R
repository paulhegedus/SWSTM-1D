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

## The Outputter class generator makes a outputter object that saves
## data. Plot output will be generated later.

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
    soilModListOP = NULL,  
    swstm1d_op = NULL, 
    
    initialize = function(modPath, ioPath, tInName, zInName, modsIn) {
      tDat <- fread(paste0(ioPath, "/inputs/",tInName,".csv")) %>%
        as.data.frame()
      zDat <- fread(paste0(ioPath, "/inputs/",zInName,".csv")) %>%
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
        # Cannot have no modules selected
        !is.null(modsIn) 
      )
      if (!all(is.character(modsIn))) {
        modsIn <-  as.character(modsIn) 
      }
      self$soilModList <- as.list(modsIn) %>% 
        `names<-`(modsIn) 
      self$soilModListOP <- as.list(paste0(modsIn, "_OP")) %>% 
        `names<-`(paste0(modsIn, "_OP")) 
    }, 
    
    SetUp = function() {
      # 1) The 'outputs' folder has to be created based on initial user inputs
      self$swstm1d_op <- SWSTM1D_OP$new(self$soilModData)
      # 2) Modules have to be loaded and initialized from the 'modules' folder
      self$soilModList <- lapply(
        self$soilModList,  
        private$.LoadModules 
      )
      # 3) Same process for the outputters, but no need to source again
      self$soilModListOP <- lapply(
        self$soilModListOP,  
        private$.LoadModules,  # Loads module outputters
        FALSE 
      )
      # 4) SoilModData structures have to be updated based on loaded modules
      lapply(
        self$soilModList,  
        private$.SetUpModules 
      )
      # 5) SoilProfile made after the SoilModData object is modified 
      self$soilModData$BuildSoilProfile()
      # 6) The initial soil profile at t=0 needs to be saved
      self$swstm1d_op$Zsave_t(0)
    }, 
    
    Execute = function() {
      # Used mapply to concurrently run each module and associated outputter 
      for (t in 1:nrow(self$soilModData$tDat)) {
        mapply(
          private$.RunModules,  
          self$soilModList, 
          self$soilModListOP,  
          MoreArgs = list(t = t) 
        )
        # Uses general outputter to save depth level data for timesteps
        self$swstm1d_op$Zsave_t(t)
        # Uses general outputter to save depth level plots for timesteps 
        self$swstm1d_op$Zplot_t(t)
        # Module specific z level plots @ each t already saved in .RunModules()
      }
    }, 
    
    Output = function() {
      # Saves the last time step to complete the time level output table
      self$swstm1d_op$Tsaves()
      # Create all time based plots across time for specified modules
      self$swstm1d_op$Tplots() 
      # Make module specific plots across simulation length (T)
      lapply(
        self$soilModListOP, 
        private$.TPlots 
      ) %>% 
        invisible()
    }
  ), 
  
  private = list(
    .LoadModules = function(module, source_module = TRUE) {
      # 1) Have to source first time running per session
      if (source_module) {
        source(paste0(self$soilModData$modPath, "/modules/", module, ".R"))
      }
      # 2) Have to initialize module based on SoilModData
      module <- eval(parse(text = paste0(module, "$new(self$soilModData)")))
      return(module)
    },  
    
    .SetUpModules = function(module) {
      module$SetUp() # module specific setup
    }, 
    
    .RunModules = function(module, module_op, t) { 
      module$Execute(t) 
      module$Update(t) 
      module_op$Zsave_t(t) 
      module_op$Zplot_t(t) 
    }, 
    
    .TPlots = function(module_op) {
      module_op$Tplots() 
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

# File OutPutter Class Generator ---------------------------
## Outputter that 
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
      private$.MakeOutputsFolder()
    },
    
    # Depth level outputter that saves data for each timestep 
    ## TODO: import the previously saved zDat data and append this with bind_rows()
    Zsave_t = function(t) {
      zDat_append <- do.call(rbind.data.frame,
                             lapply(self$soilModData$soilProfile$soilLayers,
                                    as.data.frame))
      zDat_append$time <- t
      fwrite(zDat_append, 
             paste0(self$soilModData$ioPath, "/outputs/zOut/ZxT/zDat_t", t, ".csv"))
    },
    
    
    
    # Need to save time level data after simulation ends
    Tsaves = function() {
      fwrite(self$soilModData$tDat,
             paste0(self$soilModData$ioPath, "/outputs/tOut/tDat_T.csv"))
    },
    
    # Add plots for t level data across sim that aren't module specific here
    Tplots = function() {
      private$.Plot_PxT()
      
    }
  ),
  
  private = list(
    .MakeOutputsFolder = function() {
      owd <- paste0(self$soilModData$ioPath, "/outputs") 
      if (!file.exists(owd)) { 
        dir.create(owd)
        dir.create(paste0(owd, "/tOut"))
        dir.create(paste0(owd, "/zOut"))
        dir.create(paste0(owd, "/zOut/ZxT"))
      } else {
        if (!file.exists(paste0(owd, "/tOut"))) { 
          dir.create(paste0(owd, "/tOut"))
        }
        if (!file.exists(paste0(owd, "/zOut"))) { 
          dir.create(paste0(owd, "/zOut"))
          # This folder is for plots by depth for each timestep
          if (!file.exists(paste0(owd, "/zOut/ZxT"))) { 
            dir.create(paste0(owd, "/zOut/ZxT"))
          }
        }
      }
    }
  )
)














