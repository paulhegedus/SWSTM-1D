## Title: DrainModule_Figs Outputter
## 
## Interface/Abstraction: This object follows the "outputters" 
## interface consisting of the methods; Write_z and Write_t
## Write_z: write depth (z) level data at each timestep (t)
## Write_t: writes t level data at each timestep (t)
## runOutput: does something
## closeCon: closes any open connections
##
## Description: This class is for making figures related to the drain
## module. This includes deep perc by time.

## Inputs: soilModData
## Methods: runOutput()
##
##@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@
# File OutPutter Class Generator ---------------------------
## Outputter that saves data from swstm1d simulation
DrainModule_Figs <- R6Class(
  "DrainModule_Figs",
  public = list(
    soilModData = NULL,
    t_con = NULL,
    op_ints = NULL,
    owd = NULL,
    
    initialize = function(soilModData, op_list) {
      stopifnot(
        exists("io_path", soilModData)
      ) 
      self$soilModData <- soilModData
      self$op_ints <- op_list$op_ints
      
      self$t_con <- paste0(self$soilModData$io_path, "/outputs/t_dat.csv")
      
      self$owd <- paste0(self$soilModData$io_path, "/outputs/DrainModule") 
      if (!file.exists(self$owd)) { dir.create(self$owd) }
    },
    writeZ = function(t) {},
    writeT = function(t) {},
    runOutput = function() {
      #browser()
      ## Deep Perc x Time - Bar
      df <- fread(self$t_con, header = TRUE)
      stopifnot(df$deep_perc)
      
      ymax <- RoundTo(max(df$deep_perc), 1, ceiling)
      ystep <- -(ymax-0) / 10
      xstep <- (max(df$time) - 0) / 10
      
      p <- ggplot(df, aes(x = time, y = deep_perc)) +
        geom_bar(stat = "identity",
                 color = "white",
                 fill = "darkblue") +
        scale_y_reverse(limits = c(ymax, 0),
                        labels = seq(ymax, 0, ystep),
                        breaks = seq(ymax, 0, ystep)) +
        scale_x_continuous(position = "top",
                           limits = c(0.5, max(df$time) + 0.5),
                           breaks = seq(1, max(df$time), xstep)) +
        labs(y = "Deep Percolation (units)", x = "Time (units)") +
        theme_classic()
      ggsave(filename = paste0(self$owd, "deepPerc_X_time.png"),
             plot = p,
             device = "png",
             scale = 1,
             width = 5,
             height = 7.5,
             units = "in")
      #print(p)
    },
    closeCon = function() {}
  )
  #private = list()
)







