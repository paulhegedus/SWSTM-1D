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
SoilModData <- R6Class(
  "SoilModData",
  public = list(
    # t level data
    t_dat = NULL,
    # z level data
    z_dat = NULL,
    # soil profile
    soil_profile = NULL,
    ## initialize
    initialize = function(t_dat,z_dat){
      ## check for requirements on the bare minimum for data
      stopifnot(is.data.frame(t_dat),
                is.data.frame(z_dat),
                any(grepl("time",names(t_dat))),
                any(grepl("fc",names(z_dat))),
                any(grepl("vwc",names(z_dat))),
                any(grepl("depth",names(z_dat))),
                is.numeric(t_dat$time),
                is.numeric(z_dat$depth),
                is.numeric(z_dat$time),
                is.numeric(z_dat$fc),
                is.numeric(z_dat$vwc),
                all(z_dat$depth>0),
                all(z_dat$fc>0 & z_dat$fc<1),
                all(z_dat$vwc>0 & z_dat$vwc<1),
                length(unique(z_dat$time))==1,
                unique(z_dat$time)==0)
      ## initialize
      self$t_dat <- t_dat
      self$z_dat <- z_dat
      # add z to layers
      self$z_dat$z <- private$sumPrevNumFun(self$z_dat$depth)
    },
    ## plotting functions
    plotVWCxDepth = function(t){
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
    plotDPxTime = function(){
      stopifnot(any(grepl("deepPerc",names(self$t_dat))))
      pd <- self$t_dat
      ymax <- RoundTo(max(pd$deepPerc),1)
      ystep <- -(ymax-0)/10
      p <- ggplot(pd,
                  aes(x=time,
                      y=deepPerc)) +
        geom_bar(stat="identity",
                 color="white",
                 fill="darkblue") +
        scale_y_reverse(limits=c(ymax,0),
                        labels=seq(ymax,0,ystep),
                        breaks=seq(ymax,0,ystep)) +
        scale_x_continuous(position = "top") +
        labs(y="Deep Percolation",x="Time Step") +
        theme_classic() 
      ## TODO: add in depth & time units
      print(p)
    },
    plotPxTime = function(){
      stopifnot(any(grepl("prec",names(self$t_dat))))
      pd <- self$t_dat
      ymax <- RoundTo(max(pd$prec),1)
      ystep <- -(ymax-0)/10
      p <- ggplot(pd,
                  aes(x=time,
                      y=prec)) +
        geom_bar(stat="identity",
                 color="white",
                 fill="blue") +
        scale_y_reverse(limits=c(ymax,0),
                        labels=seq(ymax,0,ystep),
                        breaks=seq(ymax,0,ystep)) +
        scale_x_continuous(position = "top") +
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
    initialize = function(){},
    setup = function(soilModData,soilProfile){
      if(any(grepl("prec",names(soilModData$t_dat)))){
        soilProfile$soil_layers[[1]]$wTop <- soilModData$t_dat$prec[t]
      }else{
        soilProfile$soil_layers[[1]]$wTop <- 0
      }
      return(invisible(self))
    },
    calculate = function(soilProfile){
      for(i in 1:length(soilProfile$soil_layers)){
        soilProfile$soil_layers[[i]] <- private$drainFCfun(soilProfile$soil_layers[[i]])
        if(i!=length(soilProfile$soil_layers)){
          soilProfile$soil_layers[[i+1]]$wTop <- soilProfile$soil_layers[[i]]$wBot
        }
      }
      return(invisible(self))
      
    },
    update = function(soilModData,soilProfile,t){
      ## need to update the t level data
      soilModData$t_dat$deepPerc[t] <- soilProfile$soil_layers[[length(soilProfile$soil_layers)]]$wBot
      
      ## need to update the z level data
      z_dat_append <- do.call(rbind.data.frame,
                              soilProfile$soil_layers %>%
                                lapply(as.data.frame))
      z_dat_append$time <- t
      soilModData$z_dat <- bind_rows(soilModData$z_dat,z_dat_append)
      return(invisible(self))
    }
  ),
  private = list(
    drainFCfun = function(soil_layer){
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
#### Classes ####

## test case
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

FooBar <- R6Class(
  classname = "FooBar",
  public = list(
    initialize = function(){},
    funfun = function(foo){
      foo$x <- foo$x + 1
    }
  )
)
FooBar$new()$funfun(foo)






