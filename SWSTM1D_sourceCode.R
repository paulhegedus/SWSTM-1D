## SWSTM1D - Soil Water and Solute Transport Model 1D
## Source Code (Non-Module Specific Classes)
##
## Paul Hegedus
## Date: 05/04/2020 

## Description:
## Classes and functions for the 1D water and solute transort model. 
## First loads packages and functions for swstm1d
## Then checks model space for folders called 'modules' and 'inputs'.
## Next, sources all of the R6 classes not specific to modules

## Notes:
##

##@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@
#### Packages ####
##@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@
## load packages for running SWSTM1D_exe.R
library(R6)
library(tidyverse)
library(DescTools)
library(data.table)

##@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@
#### Functions ####
##@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@
## generic functions that are not class specific

# < none >

##@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@
#### Check Model Space ####
##@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@
# if no modules folder return error
if(!file.exists(paste0(getwd(),"/modules"))){
  stop("No 'modules' folder present.")
}
# if no inputs folder return error
if(!file.exists(paste0(getwd(),"/inputs"))){
  stop("No 'inputs' folder present.")
}else{
  if(!file.exists(paste0(getwd(),"/inputs/tIn_dat.csv"))){
    stop("Time level inputs named 'tIn_dat.csv' required.")
  }
  if(!file.exists(paste0(getwd(),"/inputs/zIn_dat.csv"))){
    stop("Depth (t=0) level inputs named 'zIn_dat.csv' required.")
  }
  if(!file.exists(paste0(getwd(),"/inputs/moduleSlctIn_dat.csv"))){
    stop("Column of module names required in file named 'moduleSlctIn_dat.csv'.")
  }
}

##@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@
#### SWSTM1D Class ####
##@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@
# soil water and solute transport model 1D
# takes time dataframe 'tDat', depth dataframe 'zDat', and df of module names 'modules'.
# based on these module names and data provided, the simulation can be primed/setup, and then 
# executed. 
SWSTM1D <- R6Class(
  "SWSTM1D",
  public = list(
    # attributes/fields
    soilModDat = NA, # soil model data object
    soilModList = NA,
    
    # methods
    initialize = function(tIn,zIn,mIn){
      # 1) initialize object of SoilModData class
      self$soilModDat <- SoilModData$new(
        tIn, # time level inputs
        zIn # depth level inputs
      )
      stopifnot(
        !is.null(mIn) # make sure not null
      )
      if(!all(sapply(mIn,is.character))){
        mIn$moduleNames <- mIn$moduleNames %>% 
          as.character() # all module names are changed to characters
      }
      self$soilModList <- as.list(mIn$moduleNames) %>% # make list of module names
        `names<-`(mIn$moduleNames) # make names of modList the module names
    },
    setup = function(){
      # 1) loads modules from 'modules' folder
      lapply(
        self$soilModList, # sources each module 
        private$loadModules # ?? need to initialize each module in the list here ??
      )
      # 2) amends soilModData structures based on modules
      lapply(
        self$soilModList ,
        private$setupModules # calls setup method of each module
      )
    },
    execute = function(){
      # 1) loop over every time step
      # 2) run all the modules and update outputs
      for(t in 1:nrow(self$soiModDat$tDat)){
        lapply(
          self$soilModList ,
          private$runModules, # <- ?? outputter called in here ??
          t # takes the time step
        )
        # ?? or call outputter here... or both ??
      }
    },
    output = function(){
      # 1) save module specific outputs to 'outputs' folder from each module
      lapply(
        self$soilModList ,
        private$plotModules # <- should be outputter ??
      )
    }
  ),
  private = list(
    loadModules = function(MODULE){
      # ?? make the element of the soilModList an initialized module ??
      MODULE <- source(paste0(getwd(),"/modules/",MODULE,".R")) 
      # ??
    }, 
    setupModules = function(MODULE){
      MODULE$setup() # module specific setup
    },
    runModules = function(MODULE,t){
      MODULE$execute(t) # execute module
      MODULE$update(t) # update soilModData from module
      # ?? outputter called in here ??
    },
    plotModules = function(MODULE){
      MODULE$plotGen() # generate module specific plots
    }
    # ?? ^ could be outputter ??
  )
)

##@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@
#### SoilModData Class ####
##@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@
# class that holds t (time) and z (depth) level data frames with inputs/outputs
# as well as the soilProfile object that is updated during the simulation 
# (private object?). 
SoilModData <- R6Class(
  classname = "SoilModData",
  public = list(
    ## attributes/fields
    tDat = NULL, # t-level data
    zDat = NULL,    # z level data
    soilProfile = NULL, # soil profile class

    ## methods
    initialize = function(tDat,zDat){
      # check for requirements on the bare minimum for data
      stopifnot(
        is.data.table(tDat), # table with each row a time increment
        is.data.table(zDat), # table with each row a depth increment
        any(grepl("time",names(tDat))), # col indicates time step 
        any(grepl("time",names(zDat))), # col indicates time step 
        any(grepl("depth",names(zDat))), # depth of layer increment
        is.numeric(tDat$time), # time col must be numeric
        is.numeric(zDat$time), # time col must be numeric
        is.numeric(zDat$depth), # depth col must be numeric
        all(zDat$depth>0), # depths can't be less than 0
        length(unique(zDat$time))==1, # can't have more than one time step for initial conditions
        unique(zDat$time)==0 # time step must be 0 (initial conditions). 
        # ^ not really necessary but checks that user sort of knows what they are doing
      ) 
      # initialize
      self$tDat <- tDat
      self$zDat <- zDat
      
      self$zDat$z <- private$sumPrevNumFun(self$zDat$depth) # add z (depth) to zDat
      self$soilProfile <- SoilProfile$new(self$zDat) # initialize soilProfile class w/zDat
    }
  ),
  private = list(
    sumPrevNumFun = function(vec){
      stopifnot(
        is.numeric(vec),
        length(vec)>1
      ) 
      for(i in 2:length(vec)){ 
        vec[i] <- vec[i] + vec[i-1]
      }
      return(vec)
    }
  )
)

##@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@
#### SoilProfile Class ####
##@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@
SoilProfile <- R6Class(
  "SoilProfile",
  public = list(
    soilLayers = NULL,
    initialize = function(zDat){
      # remove time column from soil profile 
      zDat$time <- NULL # irrelevant col when object used in sim
      # make soil profile
      self$soilLayers <- apply(
        zDat, 
        1,
        as.list
      )
    }
  )
)

##@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@
#### OutPutter Class ####
##@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@
















