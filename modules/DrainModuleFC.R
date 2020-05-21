## Title: DrainModuleFC
## 
## Interface/Abstraction: This object follows the "modules" 
## interface consisting of the methods;SetUp(), Execute(), Update(), plotGen()
##
## Description: This class is the instantaneous drain module for the 1d soil 
## simulation model. The exe method determines if precip available, and then 
## for every layer in the class, the function calculates how much water goes 
## in or out of each layer.
##
## Inputs: soilModData (R6 class - args: soilProfile, tDat,zDat)
## Methods: SetUp, Execute, Update, plotGen
##
##@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@
# DrainModuleFC Class Generator ---------------------------
DrainModuleFC <- R6Class(
  classname="DrainModuleFC",
  public = list(
    soilModData = NULL, 

    initialize = function(soilModData) {
      stopifnot(
        exists("tDat", soilModData),
        exists("zDat", soilModData),
        exists("ioPath", soilModData),
        file.exists(paste0(soilModData$ioPath, 
                           "/inputs/DrainModuleFC_in.csv")),
        !is.null(soilModData$zDat$vwc),
        is.numeric(soilModData$zDat$vwc), 
        all(soilModData$zDat$vwc > 0 & soilModData$zDat$vwc < 1)
      ) 
      self$soilModData <- soilModData
    },
    
    SetUp = function() {
      # 1) Modules specific data must be in folder named 'inputs'
      dfcIn <- fread(paste0(self$soilModData$ioPath, 
                            "/inputs/DrainModuleFC_in.csv")) %>%
        as.data.frame()
      stopifnot(
        is.data.frame(dfcIn),
        !is.null(dfcIn$fc),
        is.numeric(dfcIn$fc),
        nrow(dfcIn) == nrow(self$soilModData$zDat) 
      )
      # 2) Input data has to be modified
      self$soilModData$zDat$fc <- dfcIn$fc
      # 3) Output data has to be modified (0 added as defaults to avoid elses)
      self$soilModData$tDat$deepPerc <- 0 
      self$soilModData$zDat$wTop <- 0 
      self$soilModData$zDat$wBot <- 0 
    },
    
    Execute = function(t) {
      # Get the number of soil layers for shorter pointer
      num_layers <- length(self$soilModData$soilProfile$soilLayers)
      if (!is.null(self$soilModData$tDat$prec)) { 
        self$soilModData$soilProfile$soilLayers[[1]]$wTop <- 
          self$soilModData$tDat$prec[t]
      } # Else not needed b/c default set to 0
      for (i in 1:num_layers) {
        self$soilModData$soilProfile$soilLayers[[i]] <- 
          private$.DrainFunFC(self$soilModData$soilProfile$soilLayers[[i]])
        if (i != num_layers) {
          self$soilModData$soilProfile$soilLayers[[i+1]]$wTop <- 
            self$soilModData$soilProfile$soilLayers[[i]]$wBot
        }
      }
    },
    
    Update = function(t) {
      # Get the number of soil layers for shorter pointer
      num_layers <- length(self$soilModData$soilProfile$soilLayers)
      self$soilModData$tDat$deepPerc[t] <- 
        self$soilModData$soilProfile$soilLayers[[num_layers]]$wBot
    }
  ),
  
  private = list(
    .DrainFunFC = function(soilLayer) {
      soilLayer$vwc <- soilLayer$vwc + soilLayer$wTop
      if (soilLayer$vwc > soilLayer$fc) {
        soilLayer$wBot <- soilLayer$vwc - soilLayer$fc * soilLayer$depth
        soilLayer$vwc <- soilLayer$vwc - soilLayer$wBot
      } # Else not needed b/c default set to 0 
      return(soilLayer)
    }
  )
)


# DrainModuleFC Outputter Class Generator ---------------------------
DrainModuleFC_OP <- R6Class(
  "DrainModuleFC_OP",
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
    
    # For saving module specific data at each t step
    Zsave_t = function(t){
      
    },
    # For plotting module specific data at each t step 
    Zplot_t = function(t){
      
    },
    # For plotting module specific data at end of sim
    Tplots = function(){
      private$.Plot_DPxT()
    } 
  ),
  private = list(
    .MakeOutputsFolder = function() {
      owd <- paste0(self$soilModData$ioPath, "/outputs/DrainModuleFC") 
      if (!file.exists(owd)) { 
        dir.create(owd)
        dir.create(paste0(owd, "/tOut"))
      }
    },
    
    # Plot of deep percolation x time is module specific
    ## TODO: add in depth & time units
    .Plot_DPxT = function(){
      stopifnot(
        any(grepl("deepPerc", names(self$soilModData$tDat)))
      )
      pd <- self$soilModData$tDat
      ymax <- RoundTo(max(pd$deepPerc), 1, ceiling)
      ystep <- -(ymax-0) / 10
      xstep <- (max(pd$time) - 0) / 10
      
      p <- ggplot(pd, aes(x = time, y = deepPerc)) +
        geom_bar(stat = "identity",
                 color = "white",
                 fill = "darkblue") +
        scale_y_reverse(limits = c(ymax, 0),
                        labels = seq(ymax, 0, ystep),
                        breaks = seq(ymax, 0, ystep)) +
        scale_x_continuous(position = "top",
                           limits = c(0.5, max(pd$time) + 0.5),
                           breaks = seq(1, max(pd$time), xstep)) +
        labs(y = "Deep Percolation", x = "Time Step") +
        theme_classic() 
      ggsave(filename = paste0(self$soilModData$ioPath, 
                               "/outputs/DrainModuleFC/tOut/DPxT.png"),
             plot = p,
             device = "png",
             scale = 1,
             width = 5, 
             height = 7.5, 
             units = "in")
      #print(p)
    }
  )
)




