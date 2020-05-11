## 1D Water and Solute Transport Model
## Paul Hegedus
## Date: 05/04/2020 

## Description:
## Classes and functions for the 1D water and solute transort model. 
## 

## Notes:
##
##@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@
#### Packages ####
library(R6)
library(tidyverse)
library(DescTools)

##@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@
### Classes ###

#### soil model class ####
# takes time dataframe 'tDat', depth dataframe 'zDat', and vector of module names 'modules'.
# based on these module names and data provided, the simulation can be primed/setup, and then 
# executed. Must have at least a drainage module specified.

# creates/holds data in a soilModData class (see SoilModData()). keeps the module vector.

# code
SoilMod1D <- R6Class(
  "SoilMod1D",
  public = list(
    ## inputs
    modules = NULL,
    ## create objects
    soilModData = NULL,
    ## initializer
    initialize = function(tDat,zDat,modules){
      stopifnot(
        any(grepl("DrainModule",modules)), # must have a drainage module selected
        length(which(grepl("DrainModule",modules)==TRUE))==1 # only 1 drain module allowed
        # ... add more conditions with more module options ...
      )
      self$modules <- modules
      self$soilModData <- SoilModData$new(tDat,zDat)
    },
    ## set up simulation
      ## ***** TODO *******
      ## need to put a set up fun... so need to rewrite set up functions
      ## if looped through t need to preallocate space, need to rewrite data object?
      ## soilProfile object only holds data at one time step and output tables and soil
      ## profile object updated by each module. 

    ## run simulations
    simRunFun = function(){
      for(t in 1:nrow(self$soilModData$tDat)){
        drainModuleFC <- DrainModuleFC$new(self$soilModData,t)
        
        ## drainage module
        if(any(grepl("DrainModuleFC",self$modules))){
          drainModuleFC <- drainModuleFC$setup()
          drainModuleFC <- drainModuleFC$calculate()
          drainModuleFC <- drainModuleFC$update()
        }
        if(any(grepl("DrainModuleRichards",self$modules))){
          # do richards stuff
        }
        ## next models
        # etc.
      }
      return(invisible(self))
    }
    
    ## ROB EDITS
    #simRunFun = function(){
    #  drainModuleFC <- DrainModuleFC$new(self$soilModData,t)
    #  for(t in 1:nrow(self$soilModData$tDat)){
    #    ## drainage module
    #    if(any(grepl("DrainModuleFC",self$modules))){
    #      drainModuleFC <- drainModuleFC$setup()
    #      drainModuleFC <- drainModuleFC$calculate()
    #      drainModuleFC <- drainModuleFC$update()
    #    }
    #    if(any(grepl("DrainModuleRichards",self$modules))){
          # do richards stuff
    #    }
        ## next models
        # etc.
    #  }
    #  return(invisible(self))
    #}
    ## output simulation level figures and tables
    
    
    
  ),
  ## private functions
  private = list()
)

SoilModData <- R6Class(
  "SoilModData",
  public = list(
    # t level data
    tDat = NULL,
    # z level data
    zDat = NULL,
    # soil profile
    soilProfile = NULL, # camelCase because this is an instantiation of a class (more than just a field)
    ## initialize
    initialize = function(tDat,zDat){
      ## check for requirements on the bare minimum for data
      stopifnot(
        is.data.frame(tDat),
        is.data.frame(zDat),
        any(grepl("time",names(tDat))),
        any(grepl("depth",names(zDat))),
        is.numeric(tDat$time),
        is.numeric(zDat$time), # *
        is.numeric(zDat$depth),
        all(zDat$depth>0),
        length(unique(zDat$time))==1, # can't have more than one time step for initial conditions
        unique(zDat$time)==0
      ) # initial conditions have to have t=0
      ## initialize
      self$tDat <- tDat
      self$zDat <- zDat
      # add z to layers
      self$zDat$z <- private$sumPrevNumFun(self$zDat$depth)
      self$soilProfile <- SoilProfile$new(self$zDat)
    },
    ## plotting functions
    plot_vwcXz = function(t){
      stopifnot(any(grepl("vwc",names(self$zDat))))
      pd <- subset(self$zDat,
                   self$zDat$time==t)
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
      print(p)
    },
    plot_dpXt = function(){
      stopifnot(any(grepl("deepPerc",names(self$tDat))))
      pd <- self$tDat
      ymax <- RoundTo(max(pd$deepPerc),1,ceiling)
      ystep <- -(ymax-0)/10
      xstep <- (max(pd$time)-0)/10
      
      p <- ggplot(pd,
                  aes(x=time,
                      y=deepPerc)) +
        geom_bar(stat="identity",
                 color="white",
                 fill="darkblue") +
        scale_y_reverse(limits=c(ymax,0),
                        labels=seq(ymax,0,ystep),
                        breaks=seq(ymax,0,ystep)) +
        scale_x_continuous(position = "top",
                           limits=c(0.5,max(pd$time)+0.5),
                           breaks = seq(1,max(pd$time),xstep)) +
        labs(y="Deep Percolation",x="Time Step") +
        theme_classic() 
      ## TODO: add in depth & time units
      print(p)
    },
    plot_pXt = function(){
      stopifnot(any(grepl("prec",names(self$tDat))))
      pd <- self$tDat
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
      print(p)
    }
  ),
  private = list(
    sumPrevNumFun = function(vec){
      stopifnot(is.numeric(vec),
                length(vec)>1) 
      for(i in 2:length(vec)){ 
        vec[i] <- vec[i] + vec[i-1]
      }
      return(vec)
    }
  )
)

SoilProfile <- R6Class(
  "SoilProfile",
  #lock_objects = FALSE,
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

DrainModuleFC <- R6Class(
  classname="DrainModuleFC",
  public = list(
    soilModData = NULL, # camelCase because a class and not just field
    t = NULL, # have to know the timestep
    initialize = function(soilModData,t){
      stopifnot(exists("tDat",soilModData),
                exists("zDat",soilModData),
                exists("soilProfile",soilModData),
                is.data.frame(soilModData$tDat), # redundant to initializer in SoilModData?
                is.data.frame(soilModData$zDat), # redundant to initializer in SoilModData?
                any(grepl("time",names(soilModData$tDat))), # redundant to initializer in SoilModData?
                any(grepl("fc",names(soilModData$zDat))),
                any(grepl("vwc",names(soilModData$zDat))), 
                any(grepl("depth",names(soilModData$zDat))), # redundant to initializer in SoilModData?
                is.numeric(soilModData$tDat$time), # redundant to initializer in SoilModData?
                is.numeric(soilModData$zDat$depth), # redundant to initializer in SoilModData?
                is.numeric(soilModData$zDat$time), # redundant to initializer in SoilModData?
                is.numeric(soilModData$zDat$fc),
                is.numeric(soilModData$zDat$vwc), 
                all(soilModData$zDat$depth>0), # redundant to initializer in SoilModData?
                all(soilModData$zDat$vwc>0 & soilModData$zDat$vwc<1),
                exists("soilLayers",soilModData$soilProfile),
                is.numeric(t)) #
      self$soilModData <- soilModData
      self$t <- t
    },
    setup = function(){
      ##**** TODO 
      ## make a setter function for adding columns
      ## make it generala... addCol <- function(df,newColName,default)
      
      # would have to 
      if(any(grepl("prec",names(self$soilModData$tDat)))){
        self$soilModData$soilProfile$soilLayers[[1]]$wTop <- self$soilModData$tDat$prec[self$t]
      }else{
        
        self$soilModData$soilProfile$soilLayers[[1]]$wTop <- 0
      }
      return(invisible(self))
    },
    calculate = function(){
      for(i in 1:length(self$soilModData$soilProfile$soilLayers)){
        self$soilModData$soilProfile$soilLayers[[i]] <- private$drainFCfun(self$soilModData$soilProfile$soilLayers[[i]])
        if(i!=length(self$soilModData$soilProfile$soilLayers)){
          self$soilModData$soilProfile$soilLayers[[i+1]]$wTop <- self$soilModData$soilProfile$soilLayers[[i]]$wBot
        }
      }
      return(invisible(self))
      
    },
    update = function(){
      ## update the t level data
      self$soilModData$tDat$deepPerc[self$t] <- self$soilModData$soilProfile$soilLayers[[length(self$soilModData$soilProfile$soilLayers)]]$wBot
      
      ## update the z level data
      zDat_append <- do.call(rbind.data.frame,
                              self$soilModData$soilProfile$soilLayers %>%
                                lapply(as.data.frame))
      zDat_append$time <- self$t
      self$soilModData$zDat <- bind_rows(self$soilModData$zDat,zDat_append)
      
      return(invisible(self))
    }
  ),
  private = list(
    drainFCfun = function(soilLayer){
      soilLayer$vwc <- soilLayer$vwc + soilLayer$wTop
      if(soilLayer$vwc > soilLayer$fc){
        soilLayer$wBot <- soilLayer$vwc - soilLayer$fc * soilLayer$depth
        soilLayer$vwc <- soilLayer$vwc - soilLayer$wBot
      }else{
        soilLayer$wBot <- 0
      }
      return(soilLayer)
    }
  )
)

##@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@
#### sandbox ####

## mutability tests
Foo <- R6Class(
  "Foo",
  public = list(
    x=NULL,
    initialize = function(x){
      self$x = x
    }
  )
)
foo <- Foo$new(5)
foo$x
FooBarNoFlds <- R6Class(
  classname = "FooBarNoFlds",
  public = list(
    initialize = function(){},
    funfun = function(foo){
      foo$x <- foo$x + 1
    }
  )
)
FooBarNoFlds$new()$funfun(foo)
foo$x

## i thought this should work
foo2 <- Foo$new(5)
FooBarWithFlds <- R6Class(
  classname = "FooBarWithFlds",
  public = list(
    x = NULL,
    initialize = function(x){
      self$x <- x
    },
    funfun = function(){
      self$x <- self$x + 1
    }
  )
)
FooBarWithFlds$new(foo2$x)$funfun()
foo2$x
# ^ not passing entire class in so makes a copy?

## this works
foo3 <- Foo$new(5)
FooBarWithFlds <- R6Class(
  classname = "FooBarWithFlds",
  public = list(
    y = NULL,
    initialize = function(y){
      self$y <- y
    },
    funfun = function(){
      self$y$x <- self$y$x + 1
    }
  )
)
FooBarWithFlds$new(foo3)$funfun()
foo3







