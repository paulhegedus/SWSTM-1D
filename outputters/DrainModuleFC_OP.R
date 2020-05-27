## Title: DrainModuleFC Outputter
## 
## Interface/Abstraction: This object follows the "outputters" 
## interface consisting of the methods; 
##
## Description: This class is the instantaneous drain module outputter for the 1d soil 
## simulation model. This is related to the DrainModuleFC class
##
## Inputs: 
## Methods: 
##
##@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@
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
    # .Plot_DPxT = function(){
    #   stopifnot(
    #     any(grepl("deepPerc", names(self$soilModData$tDat)))
    #   )
    #   pd <- self$soilModData$tDat
    #   ymax <- RoundTo(max(pd$deepPerc), 1, ceiling)
    #   ystep <- -(ymax-0) / 10
    #   xstep <- (max(pd$time) - 0) / 10
    #   
    #   p <- ggplot(pd, aes(x = time, y = deepPerc)) +
    #     geom_bar(stat = "identity",
    #              color = "white",
    #              fill = "darkblue") +
    #     scale_y_reverse(limits = c(ymax, 0),
    #                     labels = seq(ymax, 0, ystep),
    #                     breaks = seq(ymax, 0, ystep)) +
    #     scale_x_continuous(position = "top",
    #                        limits = c(0.5, max(pd$time) + 0.5),
    #                        breaks = seq(1, max(pd$time), xstep)) +
    #     labs(y = "Deep Percolation", x = "Time Step") +
    #     theme_classic() 
    #   ggsave(filename = paste0(self$soilModData$ioPath, 
    #                            "/outputs/DrainModuleFC/tOut/DPxT.png"),
    #          plot = p,
    #          device = "png",
    #          scale = 1,
    #          width = 5, 
    #          height = 7.5, 
    #          units = "in")
    #   #print(p)
    # }
  )
)

















