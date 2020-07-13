## Title:WaterContent_Figs Outputter
## 
## Interface/Abstraction: This object follows the "outputters" 
## interface consisting of the methods; Write_z and Write_t
## Write_z: write depth (z) level data at each timestep (t)
## Write_t: writes t level data at each timestep (t)
## runOutput: does something
## closeCon: closes any open connections
##
## Description: This class is for making figures related to water content
## module. This includes vwc by time and depth

## Inputs: soilModData
## Methods: runOutput()
##
##@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@
# File OutPutter Class Generator ---------------------------
## Outputter that saves data from swstm1d simulation
WaterContent_Figs <- R6Class(
  "WaterContent_Figs",
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
      
      self$owd <- paste0(self$soilModData$io_path, "/outputs/WaterContent/") 
      if (!file.exists(self$owd)) { dir.create(self$owd) }
    },
    writeZ = function(t) {},
    writeT = function(t) {},
    runOutput = function() {
      ## Deep Perc x Time - Bar
      df <- fread(self$t_con, header = TRUE)
      df <- df[-1, ]
      stopifnot(!is.null(df$deep_perc))
      
      p <- private$.plotDeepPercBar(df)
      ggsave(filename = paste0(self$owd, "deepPerc_X_time.png"),
             plot = p,
             device = "png",
             width = 5,
             height = 7.5,
             units = "in")
      #print(p)
    },
    closeCon = function() {}
  ),
  private = list(
    .plotDeepPercBar = function(df) {
      ymax <- RoundTo(max(df$deep_perc), 1, ceiling)
      ystep <- -ymax / 10
      xmax <- max(df$time) + 0.25 # for plotting xmax w/ inverse y axis
      xmin <- min(df$time) - 0.25 # see ^
      
      # if the maximum timestep is greater than 1 use integers for scale
      # else use decimals in scale (e.g tmax = 0.9 years by 0.1 intervals)
      xstep <- ifelse(max(df$time) > 1,
                      ceiling(max(df$time) / 10),
                      max(df$time) / 10)
      
      p <- ggplot(df, aes(x = time, y = deep_perc)) +
        geom_bar(stat = "identity",
                 color = "white",
                 fill = "darkblue",
                 width = 0.5) +
        scale_y_reverse(limits = c(ymax, 0),
                        labels = seq(ymax, 0, ystep),
                        breaks = seq(ymax, 0, ystep)) +
        scale_x_continuous(position = "top",
                           limits = c(xmin, xmax),
                           breaks = seq(min(df$time), max(df$time), xstep)) +
        labs(y = "Deep Percolation (units)", x = "Time (units)") +
        theme_bw()
      return(p)
    }
  )
)







