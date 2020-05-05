## 1D Water and Solute Transport Model
## Paul Hegedus

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
SoilMod1D <- R6Class(
  "SoilMod1D",
  public = list(
    soilModData = NULL,
    modules = NULL,
    initialize = function(t_in,z_in,modules){
      stopifnot(
        any(grepl("DrainModule",modules)), # must have a drainage module selected
        length(which(grepl("DrainModule",modules)==TRUE))==1 # only 1 drain module allowed
        # ... add more conditions with more module options ...
      )
      self$modules <- modules
      self$soilModData <- SoilModData$new(t_in,z_in)
    },
    ## ***** TODO *******
    ## need to put a set up fun... so need to rewrite set up functions
    
    run_fun = function(){
      for(t in 1:nrow(self$soilModData$t_dat)){
        ## drainage module
        if(any(grepl("DrainModuleFC",self$modules))){
          DrainModuleFC$
            new(self$soilModData,t)$
            setup()$
            calculate()$
            update()
        }
        if(any(grepl("DrainModuleRichards",self$modules))){
          # do richards stuff
        }
        ## next models
        # etc.
      }
      return(invisible(self))
    }
  )
)

SoilModData <- R6Class(
  "SoilModData",
  public = list(
    # t level data
    t_dat = NULL,
    # z level data
    z_dat = NULL,
    # soil profile
    soilProfile = NULL, # camelCase because this is an instantiation of a class (more than just a field)
    ## initialize
    initialize = function(t_dat,z_dat){
      ## check for requirements on the bare minimum for data
      stopifnot(
        is.data.frame(t_dat),
        is.data.frame(z_dat),
        any(grepl("time",names(t_dat))),
        any(grepl("depth",names(z_dat))),
        is.numeric(t_dat$time),
        is.numeric(z_dat$time), # *
        is.numeric(z_dat$depth),
        all(z_dat$depth>0),
        length(unique(z_dat$time))==1, # can't have more than one time step for initial conditions
        unique(z_dat$time)==0
      ) # initial conditions have to have t=0
      ## initialize
      self$t_dat <- t_dat
      self$z_dat <- z_dat
      # add z to layers
      self$z_dat$z <- private$sum_prev_num_fun(self$z_dat$depth)
      self$soilProfile <- SoilProfile$new(self$z_dat)
    },
    ## plotting functions
    plot_vwcXz = function(t){
      stopifnot(any(grepl("vwc",names(self$z_dat))))
      pd <- subset(self$z_dat,
                   self$z_dat$time==t)
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
      stopifnot(any(grepl("deepPerc",names(self$t_dat))))
      pd <- self$t_dat
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
      stopifnot(any(grepl("prec",names(self$t_dat))))
      pd <- self$t_dat
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
    sum_prev_num_fun = function(vec){
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
    soil_layers = NULL,
    initialize = function(z_dat){
      # remove time column from soil profile 
      # b/c irrelevant when updated
      z_dat$time <- NULL
      # make soil profile
      self$soil_layers <-  apply(z_dat,1,as.list)
    }
  )
)

DrainModuleFC <- R6Class(
  classname="DrainModuleFC",
  public = list(
    soilModData = NULL, # camelCase because a class and not just field
    t = NULL, # have to know the timestep
    initialize = function(soilModData,t){
      stopifnot(exists("t_dat",soilModData),
                exists("z_dat",soilModData),
                exists("soilProfile",soilModData),
                is.data.frame(soilModData$t_dat), # redundant to initializer in SoilModData?
                is.data.frame(soilModData$z_dat), # redundant to initializer in SoilModData?
                any(grepl("time",names(soilModData$t_dat))), # redundant to initializer in SoilModData?
                any(grepl("fc",names(soilModData$z_dat))),
                any(grepl("vwc",names(soilModData$z_dat))), 
                any(grepl("depth",names(soilModData$z_dat))), # redundant to initializer in SoilModData?
                is.numeric(soilModData$t_dat$time), # redundant to initializer in SoilModData?
                is.numeric(soilModData$z_dat$depth), # redundant to initializer in SoilModData?
                is.numeric(soilModData$z_dat$time), # redundant to initializer in SoilModData?
                is.numeric(soilModData$z_dat$fc),
                is.numeric(soilModData$z_dat$vwc), 
                all(soilModData$z_dat$depth>0), # redundant to initializer in SoilModData?
                all(soilModData$z_dat$vwc>0 & soilModData$z_dat$vwc<1),
                exists("soil_layers",soilModData$soilProfile),
                is.numeric(t)) #
      self$soilModData <- soilModData
      self$t <- t
    },
    setup = function(){
      ##**** TODO 
      ## make a setter function for adding columns
      ## make it generala... addCol <- function(df,newColName,default)
      if(any(grepl("prec",names(self$soilModData$t_dat)))){
        self$soilModData$soilProfile$soil_layers[[1]]$wTop <- self$soilModData$t_dat$prec[self$t]
      }else{
        
        self$soilModData$soilProfile$soil_layers[[1]]$wTop <- 0
      }
      return(invisible(self))
    },
    calculate = function(){
      for(i in 1:length(self$soilModData$soilProfile$soil_layers)){
        self$soilModData$soilProfile$soil_layers[[i]] <- private$drain_fc_fun(self$soilModData$soilProfile$soil_layers[[i]])
        if(i!=length(self$soilModData$soilProfile$soil_layers)){
          self$soilModData$soilProfile$soil_layers[[i+1]]$wTop <- self$soilModData$soilProfile$soil_layers[[i]]$wBot
        }
      }
      return(invisible(self))
      
    },
    update = function(){
      ## update the t level data
      self$soilModData$t_dat$deepPerc[self$t] <- self$soilModData$soilProfile$soil_layers[[length(self$soilModData$soilProfile$soil_layers)]]$wBot
      
      ## update the z level data
      z_dat_append <- do.call(rbind.data.frame,
                              self$soilModData$soilProfile$soil_layers %>%
                                lapply(as.data.frame))
      z_dat_append$time <- self$t
      self$soilModData$z_dat <- bind_rows(self$soilModData$z_dat,z_dat_append)
      
      return(invisible(self))
    }
  ),
  private = list(
    drain_fc_fun = function(soil_layer){
      soil_layer$vwc <- soil_layer$vwc + soil_layer$wTop
      if(soil_layer$vwc > soil_layer$fc){
        soil_layer$wBot <- soil_layer$vwc - soil_layer$fc * soil_layer$depth
        soil_layer$vwc <- soil_layer$vwc - soil_layer$wBot
      }else{
        soil_layer$wBot <- 0
      }
      return(soil_layer)
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







