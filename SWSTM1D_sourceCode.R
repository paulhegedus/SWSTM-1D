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
checkForModelReqs <- function(mod_path, io_path) {
  if (!file.exists(paste0(mod_path, "/modules"))) {
    stop("Path to 'modules' empty.")
  }
  if (!file.exists(paste0(io_path, "/inputs"))) {
    stop("Path to 'inputs' empty.")
  } else {
    if (!file.exists(paste0(io_path, "/inputs/tIn_dat.csv"))) {
      stop("Time level inputs ('tIn_dat.csv') not found.")
    }
    if (!file.exists(paste0(io_path, "/inputs/zIn_dat.csv"))) {
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
    soil_mod_list = NULL, 
    module_list = NULL,
    outputter_list = NULL,
    op_list = NULL,  

    initialize = function(mod_path, 
                          io_path, 
                          t_dat_name, 
                          z_dat_name, 
                          module_list,
                          outputter_list) {
      t_dat <- fread(paste0(io_path, "/inputs/", t_dat_name, ".csv")) %>%
        as.data.frame()
      
      z_dat <- fread(paste0(io_path, "/inputs/", z_dat_name, ".csv")) %>%
        as.data.frame()
      # 1) SoilModData class object has to be initialized first
      self$soilModData <- SoilModData$new(
        t_dat = t_dat,  
        z_dat = z_dat, 
        mod_path = mod_path,  
        io_path = io_path 
      )
      # 2) Lists for modules & outputters have to be generated from user input
      stopifnot(
        !is.null(module_list)
      )
      self$module_list <- module_list
      self$outputter_list <- outputter_list
    }, 
    setUp = function() {
      # 1) The 'outputs' folder has to be created based on initial user inputs
      private$.makeOutputsFolder()
      # 2) Modules have to be loaded and initialized from the 'modules' folder
      self$soil_mod_list <- lapply(self$module_list, private$.loadModules)
      # 3) SoilModData structures have to be updated based on loaded modules
      lapply(self$soil_mod_list, private$.setUpModules)
      # 4) SoilProfile made after the SoilModData object is modified 
      self$soilModData$buildSoilProfile()
      # 5) Outputters loaded after modules built
      self$op_list <- lapply(
        self$outputter_list,
        private$.loadOutputters
      )
    }, 
    execute = function() {
      for (t in 1:nrow(self$soilModData$t_dat)) {
        lapply(self$soil_mod_list, private$.runModules, t)
        lapply(self$op_list, private$.runZToutputters, t)
        lapply(self$op_list, private$.runToutputters, t)
      }
    }, 
    output = function() {
      lapply(self$op_list, private$.runOutputs)
      lapply(self$op_list, private$.closeConnections)
    }
  ), 
  
  private = list(
    .makeOutputsFolder = function() {
      owd <- paste0(self$soilModData$io_path, "/outputs") 
      if (!file.exists(owd)) { 
        dir.create(owd)
      } 
    },
    .loadModules = function(module_item) {
      # 1) Have to source file
      source(paste0(self$soilModData$mod_path, "/modules/", 
                    module_item$module, ".R"))
      # 2) Have to initialize module based on SoilModData
      init_text <- "$new(self$soilModData, module_item)"
      return(eval(parse(text = paste0(module_item$module, init_text))))
    },  
    .setUpModules = function(module) {
      module$setUp() # module specific setup
    },
    .loadOutputters = function(op_list) {
      # 1) Have to source file
      source(paste0(self$soilModData$mod_path, "/outputters/", 
                    op_list$op, ".R"))
      # 2) Have to initialize outputter based on SoilModData
      return(eval(parse(text = paste0(op_list$op, 
                                      "$new(self$soilModData, op_list)"))))
    }, 
    .runModules = function(module, t) { 
      module$execute(t) 
      module$update(t) 
    },
    .runZToutputters = function(module_op, t) { 
      module_op$writeZ(t) 
    },
    .runToutputters = function(module_op, t) { 
      module_op$writeT(t) 
    },
    .closeConnections = function(outputter) {
      outputter$closeCon()
    },
    .runOutputs = function(outputter) {
      outputter$runOutput()
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
    t_dat = NULL,  
    z_dat = NULL,   
    soilProfile = NULL, 
    mod_path = NULL,  
    io_path = NULL,  
    
    initialize = function(t_dat, z_dat, mod_path, io_path) {
      stopifnot(
        is.character(mod_path), 
        is.character(io_path), 
        is.data.frame(t_dat),  
        is.data.frame(z_dat),  
        any(grepl("time", names(t_dat))), 
        any(grepl("time", names(z_dat))),  
        any(grepl("thickness", names(z_dat))),  
        is.numeric(t_dat$time), 
        is.numeric(z_dat$time),  
        is.numeric(z_dat$thickness),  
        all(z_dat$thickness > 0),  
        length(unique(z_dat$time)) == 1,  
        unique(z_dat$time) == 0 # Checks that user knows what they're inputting
      ) 
      names(t_dat)[grep("thickness",names(t_dat))] <- "thiccness"
      self$t_dat <- t_dat
      self$z_dat <- z_dat
      self$mod_path <- mod_path
      self$io_path <- io_path
      
      self$z_dat$z <- cumsum(self$z_dat$thickness)
    }, 
    
    buildSoilProfile = function() {
      self$soilProfile <- SoilProfile$new(self$z_dat)
    }
  )#, 
  
  #private = list()
)

# SoilProfile Class Generator ---------------------------
SoilProfile <- R6Class(
  "SoilProfile", 
  public = list( 
    soil_layers = NULL, 
    
    initialize = function(z_dat) { 
      z_dat$time <- NULL
      self$soil_layers <- apply(z_dat, 1, as.list)
    }
  ) 
) 
















