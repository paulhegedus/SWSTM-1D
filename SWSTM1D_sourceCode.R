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
checkForModelReqs <- function(modPath,ioPath){
  # if no modules folder return error
  if(!file.exists(paste0(modPath,"/modules"))){
    stop("Path to 'modules' empty.")
  }
  # if no inputs folder return error
  if(!file.exists(paste0(ioPath,"/inputs"))){
    stop("Path to 'inputs' empty.")
  }else{
    if(!file.exists(paste0(ioPath,"/inputs/tIn_dat.csv"))){
      stop("Time level inputs ('tIn_dat.csv') not found.")
    }
    if(!file.exists(paste0(ioPath,"/inputs/zIn_dat.csv"))){
      stop("Depth (t=0) level inputs ('zIn_dat.csv') not found.")
    }
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
    soilModData = NULL, # soil model data object
    soilModList = NULL, # list of modules 
    
    
    # methods
    initialize = function(modPath,ioPath,mIn){
      # 1) initialize object of SoilModData class
      self$soilModData <- SoilModData$new(
        # time level inputs
        tDat = fread(
          paste0(
            ioPath
            ,"/inputs/tIn_dat.csv"
          )
        ) %>%
          as.data.frame(), 
        # depth level inputs
        zDat = fread(
          paste0(
            ioPath,
            "/inputs/zIn_dat.csv"
          )
        ) %>%
          as.data.frame(),
        modPath = modPath, # path to model
        ioPath = ioPath # path to inputs/outputs
      )
      # 3) make module list
      stopifnot(
        !is.null(mIn) # make sure not null
      )
      if(!all(is.character(mIn))){
        mIn <-  as.character(mIn) # all module names are changed to characters
      }
      self$soilModList <- as.list(mIn) %>% # make list of module names
        `names<-`(mIn) # make names of modList the module names
    },
    setup = function(){
      # 1) loads & intializes modules from 'modules' folder
      self$soilModList <- lapply(
        self$soilModList, 
        private$loadModules # sources each module & initializes
      )
      # 2) amends soilModData structures based on modules
      lapply(
        self$soilModList ,
        private$setupModules # calls setup method of each module
      )
      # 3) make soil profile 
      # this is done after modules amend and fill in tDat and zDat b/c soilProfile
      # made of zDat and updates zDat thru sim
      self$soilModData$buildSoilProfile()
    },
    execute = function(){
      # loop over every time step & run selected modules
      for(t in 1:nrow(self$soiModDat$tDat)){
        lapply(
          self$soilModList, # list of modules
          private$runModules, # private fxn for running execute() and update() methods
          t # time step
        )
        
        ## ** OP OP OP 
        ## ** zSave_t() # save zDat @ t
        ## ** OP OP OP 
        
      }
    },
    output = function(){
      
      ## ** OP OP OP 
      ## tSave_T() # save final tDat
      
      ## genPlotGen() # general plot generation from across simulation
      
      ## lapply(modulePlots) # apply across modules and run plots_T() method 
      
      ## ** OP OP OP 
      
      
      
      ## call to outputter for path to the
      ## outputs folder and do stuff to make general plots?
      
      ## apply across module specifc outputters and make 
      ## any final plots
      #lapply(
      #  self$soilModList ,
      #  private$plotModules # ?? should be outputter instead ??
      #)
    }
  ),
  private = list(
    loadModules = function(MODULE){
      # 1) source module
      source(paste0(self$soilModData$modPath,"/modules/",MODULE,".R"))
      # 2) initialize module (check for req inputs in module setup?)
      MODULE <- eval(parse(text=paste0(MODULE,"$new(self$soilModData)")))
      return(MODULE)
    }, 
    setupModules = function(MODULE){
      MODULE$setup() # module specific setup
    },
    runModules = function(MODULE,t){ # MODULE_OP
      MODULE$execute(t) # execute module
      MODULE$update(t) # update t level soilModData from module
    #  MODULE_OP$zSave_t(t) # saves module specific zDat for t increment
    #  MODULE_OP$zPlot_t(t) # plots module specific zDat info for t incrememnt
    },
    plotModules = function(MODULE){
      #MODULE$plotGen() # ?? could be outputter ??
    }
  )
)

##@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@
#### SoilModData Class ####
##@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@
# class that holds t (time) and z (depth) level data frames with inputs/outputs
# as well as the soilProfile object that is updated during the simulation 
# (private object?). This also holds the path to modules and inputs/outputs
# so that when modules are instantiated with this class of object, they will 
# know where to look for any data specific to them
SoilModData <- R6Class(
  classname = "SoilModData",
  public = list(
    ## attributes/fields
    tDat = NULL, # t-level data
    zDat = NULL,    # z level data
    soilProfile = NULL, # soil profile class
    modPath = NULL, # path to model
    ioPath = NULL, # path to inputs/outputs
    
    ## methods
    initialize = function(tDat,zDat,modPath,ioPath){
      # check for minimum requirements 
      stopifnot(
        is.character(modPath),
        is.character(ioPath),
        is.data.frame(tDat), # table with each row a time increment
        is.data.frame(zDat), # table with each row a depth increment
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
      self$modPath <- modPath
      self$ioPath <- ioPath
      
      self$zDat$z <- private$sumPrevNumFun(self$zDat$depth) # add z (depth) to zDat
    },
    buildSoilProfile = function(){
      self$soilProfile <- SoilProfile$new(self$zDat)
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
      # b/c irrelevant when updated
      zDat$time <- NULL
      # make soil profile
      self$soilLayers <-  apply(zDat,1,as.list)
    }
  ) 
) 

##@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@
#### OutPutter Class ####
##@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@














