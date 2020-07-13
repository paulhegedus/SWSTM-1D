## Title: TranspModule_Figs Outputter
## 
## Interface/Abstraction: This object follows the "outputters" 
## interface consisting of the methods; Write_z and Write_t
## Write_z: write depth (z) level data at each timestep (t)
## Write_t: writes t level data at each timestep (t)
## runOutput: does something
## closeCon: closes any open connections
##
## Description: This class is for making figures related to the transpiration
## modules. This includes transpiration by time. 

## Inputs: soilModData
## Methods: runOutput()
##
##@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@
# File OutPutter Class Generator ---------------------------
## Outputter that saves data from swstm1d simulation
TranspModule_Figs <- R6Class(
  "TranspModule_Figs",
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
      
      # Write initial z and t level info & open connection
      self$t_con <- paste0(self$soilModData$io_path, "/outputs/t_dat.csv")
      
      self$owd <- paste0(self$soilModData$io_path, "/outputs/ET/") 
      if (!file.exists(self$owd)) { dir.create(self$owd) }
    },
    writeZ = function(t) {},
    writeT = function(t) {},
    runOutput = function() {
      df <- fread(self$t_con, header = TRUE)
      df <- df[-1, ]
      stopifnot(!is.null(df$AT_soil_zone), !is.null(df$AT_sub_soil))

      p <- private$.plotTranspBar(df)
      ggsave(filename = paste0(self$owd, "transp_X_time.png"),
             plot = p,
             device = "png",
             width = 5,
             height = 7.5,
             units = "in")
      #print(p)
     
      p <- private$.plotSSTranspBar(df)
      ggsave(filename = paste0(self$owd, "transp_subSoil_X_time.png"),
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
    .plotTranspBar = function(df) {
      ### Soil Zone Transpiration (Transp x Time - Bar)
      ymax <- RoundTo(max(df$AT_soil_zone), 1, ceiling)
      ystep <- ymax / 10
      xmax <- max(df$time) + 0.25 # for plotting xmax w/ inverse y axis
      xmin <- min(df$time) - 0.25 # see ^
      # if the maximum timestep is greater than 1 use integers for scale
      # else use decimals in scale (e.g tmax = 0.9 years by 0.1 intervals)
      xstep <- ifelse(max(df$time) > 1,
                      ceiling(max(df$time) / 10),
                      max(df$time) / 10)
      
      p <- ggplot(df, aes(x = time, y = AT_soil_zone)) +
        geom_bar(stat = "identity",
                 color = "white",
                 fill = "darkgreen",
                 width = 0.5) +
        scale_y_continuous(limits = c(0, ymax),
                           labels = seq(0, ymax, ystep),
                           breaks = seq(0, ymax, ystep)) +
        scale_x_continuous(limits = c(xmin, xmax),
                           breaks = seq(min(df$time), max(df$time), xstep)) +
        labs(y = "Transpiration (units)", x = "Time (units)") +
        theme_classic()
      return(p)
    },
    .plotSSTranspBar = function(df) {
      ### Sub-Soil Zone Transpiration (Transp x Time - Bar)
      ymax <- RoundTo(max(df$AT_sub_soil), 1, ceiling)
      ystep <- ymax / 10
      xmax <- max(df$time) + 0.25 # for plotting xmax w/ inverse y axis
      xmin <- min(df$time) - 0.25 # see ^
      # if the maximum timestep is greater than 1 use integers for scale
      # else use decimals in scale (e.g tmax = 0.9 years by 0.1 intervals)
      xstep <- ifelse(max(df$time) > 1,
                      ceiling(max(df$time) / 10),
                      max(df$time) / 10)
      
      p <- ggplot(df, aes(x = time, y = AT_sub_soil)) +
        geom_bar(stat = "identity",
                 color = "white",
                 fill = "darkgreen",
                 width = 0.5) +
        scale_y_continuous(limits = c(0, ymax),
                           labels = seq(0, ymax, ystep),
                           breaks = seq(0, ymax, ystep)) +
        scale_x_continuous(limits = c(xmin, xmax),
                           breaks = seq(min(df$time), max(df$time), xstep)) +
        labs(y = "Sub-Soil Transpiration (units)", x = "Time (units)") +
        theme_classic()
      return(p)
    }
  )
)







