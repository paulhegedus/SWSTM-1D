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
    soilModListOP = NULL, # list of module outputters
    swstm1d_op = NULL,
    
    # methods
    initialize = function(modPath,ioPath,mIn){
      # 1) initialize swstm1d general outputter
      self$
      
      # 2) initialize object of SoilModData class
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
      self$soilModListOP <- as.list(paste0(mIn,"_OP")) %>% # make list of module names
        `names<-`(paste0(mIn,"_OP")) # make names of modList the module names
    },
    setup = function(){
      # 1) make swstm1d_op (general model outputter) & 'outputs' folder
      self$swstm1d_op <- SWSTM1D_OP$new(self$soilModData)
      # 2) loads & intializes modules from 'modules' folder
      self$soilModList <- lapply(
        self$soilModList, 
        private$loadModules # sources each module & initializes
      )
      # 3) loads & intializes module ouputters from 'modules' folder (same source file)
      self$soilModListOP <- lapply(
        self$soilModListOP, 
        private$loadModules, # loads outputters
        FALSE # modules already sourced (op with r6 class generator)
      )
      # 4) amends soilModData structures based on modules
      lapply(
        self$soilModList ,
        private$setupModules # calls setup method of each module
      )
      # 5) make soil profile 
      # this is done after modules amend and fill in tDat and zDat b/c soilProfile
      # made of zDat and updates zDat thru sim
      self$soilModData$buildSoilProfile()
      # 6) save t=0 zDat
      self$swstm1d_op$zSave_t(0)
      
    },
    execute = function(){
      # loop over every time step & run selected modules
      for(t in 1:nrow(self$soiModDat$tDat)){
        mapply(
          private$runModules, # private fxn for running execute() and update() methods
          self$soilModList, # list of modules
          self$soilModListOP, # list of module outputters
          MoreArgs = list(t=t) # time step
        )
        self$swstm1d_op$zSave_t(t)
        self$swstm1d_op$zPlot_t(t)
      }
    },
    output = function(){
      ## tSave_T() # save final tDat
      self$swstm1d_op$tSave_T()
      
      ## general plot generation from swstm1d
      self$swstm1d_op$tPlot_T() 
      
      ## make module specific plots across T
      lapply(
        self$soilModListOP ,
        private$tPlots_T # calls module specific plots across T
      )
    }
  ),
  private = list(
    loadModules = function(MODULE,SOURCE = TRUE){
      # 1) source module
      if(SOURCE){
        source(paste0(self$soilModData$modPath,"/modules/",MODULE,".R"))
      }
      # 2) initialize module (check for req inputs in module setup?)
      MODULE <- eval(parse(text=paste0(MODULE,"$new(self$soilModData)")))
      return(MODULE)
    }, 
    setupModules = function(MODULE){
      MODULE$setup() # module specific setup
    },
    runModules = function(MODULE,MODULE_OP,t){ # MODULE_OP
      MODULE$execute(t) # execute module
      MODULE$update(t) # update t level soilModData from module
      MODULE_OP$zSave_t(t) # saves module specific zDat for t increment
      MODULE_OP$zPlot_t(t) # plots module specific zDat info for t incrememnt
    },
    tPlots_T = function(MODULE_OP){
      MODULE_OP$tPlot_T() # calls module specific plots across T
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
# general outputter class for the swstm1d model
SWSTM1D_OP <- R6Class(
  "SWSTM1D_OP",
  public = list(
    # fields/attributes
    soilModData = NULL,
    
    # methods
    initialize = function(soilModData){
      stopifnot(
        # check if data structures exist
        exists("tDat",soilModData),
        exists("zDat",soilModData),
        exists("ioPath",soilModData)
      ) 
      self$soilModData <- soilModData
      
      ## TODO: MAKE OUTPUTS FOLDER
      ## create path to an 'outputs' folder
      owd <- paste0(self$soilModData$ioPath,"/outputs") # outputs working directory
      if(!file.exists(owd)){ 
        dir.create(owd)
        dir.create(paste0(owd,"/outT"))
        dir.create(paste0(owd,"/outZ"))
        dir.create(paste0(owd,"/outZ/zXt"))
      }else{
        if(!file.exists(paste0(owd,"/outT"))){ 
          dir.create(paste0(owd,"/outT"))
        }
        if(!file.exists(paste0(owd,"/outZ"))){ 
          dir.create(paste0(owd,"/outZ"))
          if(!file.exists(paste0(owd,"/outZ/zXt"))){ 
            dir.create(paste0(owd,"/outZ/zXt"))
          }
        }
      }
    },
    # zDat outputter for each t (saves zDat at end of each t)
    zSave_t = function(t){
      # make zDat for time step from soil layers
      zDat_append <- do.call(rbind.data.frame,
                             self$soilModData$soilProfile$soilLayers %>%
                               lapply(as.data.frame))
      zDat_append$time <- t
      
      # save z level data for t step in outputs folder in outZ folder
      fwrite(zDat_append,paste0(self$soilModData$ioPath,"/outputs/outZ/zXt/zDat_t",t,".csv"))
    },
    # make plots by depth for time 
    zPlot_t = function(t){
      private$plot_vwcXz(t)
    },
    # tDat save after T (saves tDat after sim ends)
    tSave_T = function(){
      # save t level data in outputs folder in outT folder
      fwrite(self$soilModData$tDat,paste0(self$soilModData$ioPath,"/outputs/outT/tDat_T.csv"))
    },
    # t-level plots after sim ends
    tPlot_T = function(){
      private$plot_pXt()
    }
  ),
  private = list(
    # plot vwc by time 
    plot_vwcXz = function(t){
      owd <- paste0(self$soilModData$ioPath,"/outputs/outZ/vwcXt") # outputs working directory for module
      if(!file.exists(owd)){ 
        dir.create(owd)
      }
      stopifnot(any(grepl("vwc",names(self$soilModData$zDat))))
      pd <- subset(self$soilModData$zDat,
                   self$soilModData$zDat$time==t)
      for(i in 1:nrow(pd)){
        pd$z_labels[i] <- 
          ifelse(i==1,
                 paste0("0 - ",
                        pd$z[i]),
                 paste0(pd$z[i-1],
                        " - ",
                        pd$z[i]))
      }
      pd$z_labels <- as.factor(pd$z_labels) %>%
        fct_rev()
      p <- ggplot(pd,
                  aes(x=z_labels,y=vwc)) +
        geom_bar(stat="identity",
                 color="darkblue",
                 fill="darkblue") +
        scale_y_continuous(limits=c(0,1), 
                           breaks=seq(0,1,0.1)) +
        labs(y="Volumetric Water Content",x="Depth") +
        coord_flip() + 
        theme_classic() +
        ggtitle(paste0("Time: ",t))
      ## TODO: add in depth units
      #print(p)
      ggsave(p,paste0(self$soilModData$ioPath,"/outputs/outZ/vwcXt/vwcXz_t",t,".png"),
             device = "png",scale = 1,width = 5, height = 7.5, units = "in")
    },
    # plot precipitation by time 
    plot_pXt = function(){
      stopifnot(any(grepl("prec",names(self$soilModData$tDat))))
      pd <- self$soilModData$tDat
      ymax <- RoundTo(max(pd$prec),1,ceiling)
      ystep <- -(ymax-0)/10
      xstep <- (max(pd$time)-0)/10
      
      p <- ggplot(pd,
                  aes(x=time,
                      y=prec)) +
        geom_bar(stat="identity",
                 color="white",
                 fill="blue") +
        scale_y_reverse(limits=c(ymax,0),
                        labels=seq(ymax,0,ystep),
                        breaks=seq(ymax,0,ystep)) +
        scale_x_continuous(position = "top",
                           limits=c(0.5,max(pd$time)+0.5),
                           breaks = seq(1,max(pd$time),xstep)) +
        labs(y="Precipitation",x="Time Step") +
        theme_classic() 
      ## TODO: add in depth & time units
      #print(p)
      ggsave(p,paste0(self$soilModData$ioPath,"/outputs/outT/pXt_T.png"),
             device = "png",scale = 1,width = 5, height = 7.5, units = "in")
    }
  )
)














