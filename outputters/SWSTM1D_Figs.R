## Title: SWSTM1D_Figs Outputter
## 
## Interface/Abstraction: This object follows the "outputters" 
## interface consisting of the methods; Write_z and Write_t
## Write_z: write depth (z) level data at each timestep (t)
## Write_t: writes t level data at each timestep (t)
## runOutput: does something
## closeCon: closes any open connections
##
## Description: This class is the general figure making outputter 
## for the SWSTM-1D model. The Outputter class generator makes a 
## outputter object that saves data. Plot output will be generated later.

## Inputs: soilModData
## Methods: runOutput()
##
##@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@
# File OutPutter Class Generator ---------------------------
## Outputter that saves data from swstm1d simulation
SWSTM1D_Figs <- R6Class(
  "SWSTM1D_Figs",
  public = list(
    soilModData = NULL,
    z_con = NULL,
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
      self$z_con <- paste0(self$soilModData$io_path, "/outputs/z_dat.csv")
      self$t_con <- paste0(self$soilModData$io_path, "/outputs/t_dat.csv")
      
      self$owd <- paste0(self$soilModData$io_path, "/outputs/WaterBalance/") 
      if (!file.exists(self$owd)) { dir.create(self$owd) }
    },
    writeZ = function(t) {},
    writeT = function(t) {},
    runOutput = function() {
      # Set Up Output Data for Water Balance
      zdf <- fread(self$z_con, header = TRUE)
      zdf$w_len <- zdf$vwc * zdf$thickness
      tdf <- fread(self$t_con, header = TRUE)
      tdf$init_w_len <- NA 
      tdf$w_len[1] <- sum(zdf %>% subset(time == 0) %>% select(w_len))
      # assuming starting with equal water balance
      tdf$init_w_len <- c(tdf$w_len[1], tdf$w_len[1:(nrow(tdf) - 1)])
      zdf_wide <- pivot_wider(zdf, 
                              names_from = layer_id, 
                              names_prefix = "layer_", 
                              values_from = w_len)
      layer_cols <- 
        (ncol(zdf_wide) - (length(unique(zdf$layer_id)) - 1)):ncol(zdf_wide)
      tdf_w_len <- setDT(zdf_wide)[, lapply(.SD, 
                                            sum, 
                                            na.rm = TRUE), 
                                   by = .(time), 
                                   .SDcols = names(zdf_wide)[layer_cols]] %>%
        as.data.frame()
      tdf <- cbind(tdf, tdf_w_len[, -grep("time", names(tdf_w_len))]) 
      layer_cols <- 
        (ncol(tdf) - (length(unique(zdf$layer_id)) - 1)):ncol(tdf)
      
      for (i in 1:nrow(tdf)) {
        tdf$inputs[i] <- 
          ifelse(!is.null(tdf$prec[i]), tdf$prec[i], 0) +
          ifelse(!is.null(tdf$init_w_len[i]), tdf$init_w_len[i], 0)
        tdf$fates[i] <- 
          ifelse(!is.null(tdf$AT_soil_zone[i]), tdf$AT_soil_zone[i], 0) +
          ifelse(!is.null(tdf$AE[i]), tdf$AE[i], 0) +
          ifelse(!is.null(tdf$deep_perc[i]), tdf$deep_perc[i], 0) +
          ifelse(!is.null(tdf$runoff[i]), tdf$runoff[i], 0) +
          ifelse(!is.null(tdf$w_len[i]), tdf$w_len[i], 0)
      }
      tdf$wb_diff <- tdf$inputs - tdf$fates
      tdf$wb_diff[1] <- 0
      #tdf <- tdf[-1, ]

      #### MB I/O ####
      p <- private$.plotWBdiff(tdf)
      ggsave(filename = paste0(self$owd, "water_balance_diff.png"),
             plot = p,
             device = "png",
             width = 10,
             height = 10,
             units = "in")
      
      #### MB Inventory ####
      p <- private$.plotWBinv(tdf, layer_cols)
      ggsave(filename = paste0(self$owd, "water_inventory.png"),
             plot = p,
             device = "png",
             width = 10,
             height = 10,
             units = "in")
      
      #### MB Partition ####
      browser()
      p <- private$.plotWBpart(tdf)
      ggsave(filename = paste0(self$owd, "water_partition.png"),
             plot = p,
             device = "png",
             width = 10,
             height = 10,
             units = "in")
    },
    closeCon = function() {}
  ),
  private = list(
    .plotWBdiff = function (tdf) {
      # gather vars
      wbdf <- tdf[, c("time", "inputs", "fates", "wb_diff")] %>%
        gather(key="Process", value="units", -("time"))
      
      # scales : number to center the difference on the scale
      scl <-  floor_dec(
        min(subset(wbdf, 
                   wbdf$Process != "wb_diff")$units,
            na.rm = TRUE),
        nchar(round(1 / min(subset(wbdf, 
                                   wbdf$Process != "wb_diff")$units,
                            na.rm = TRUE)))) +
        (ceiling_dec(
          max(subset(wbdf, 
                     wbdf$Process != "wb_diff")$units,
              na.rm = TRUE),
          nchar(round(1 / max(subset(wbdf, 
                                     wbdf$Process != "wb_diff")$units, 
                              na.rm = TRUE)))) -
           floor_dec(
             min(subset(wbdf, wbdf$Process != "wb_diff")$units, na.rm = TRUE),
             nchar(round(1 / min(subset(wbdf, 
                                        wbdf$Process != "wb_diff")$units, 
                                 na.rm = TRUE))))
        ) / 2
      
      # relevel factors (3)
      wbdf[which(wbdf$Process=="wb_diff"),"units"] <-
        wbdf[which(wbdf$Process=="wb_diff"),"units"] + scl
      wbdf$Process <- factor(wbdf$Process)
      wbdf$Process = factor(wbdf$Process, levels(wbdf$Process)[c(2, 1, 3)])
      
      # plot
      ymin <- floor_dec(min(wbdf$units), nchar(round(1 / min(wbdf$units))))
      ymax <- ceiling_dec(max(wbdf$units), nchar(round(1 / max(wbdf$units))))
      ystep <- (ymax - ymin) / 10
      xmax <- max(wbdf$time) #+ 0.25 # for plotting xmax w/ inverse y axis
      xmin <- min(wbdf$time) #- 0.25 # see ^
      xstep <- ifelse(max(wbdf$time) > 1,
                      ceiling(max(wbdf$time) / 10),
                      max(wbdf$time) / 10)
      p <- ggplot(data = wbdf, aes(x = time, color = Process)) +
        geom_line(aes(y = units, linetype = Process), na.rm = TRUE) +
        scale_y_continuous(name="Water (length)", 
                           limits = c(ymin, ymax),
                           #breaks = seq(ymin, ymax, ystep), 
                           sec.axis = 
                             sec_axis(~.-scl, 
                                      name = 
                                        "Water Balance Difference (units)")) +
        scale_color_manual(name="",
                           breaks = c("inputs", "fates", "wb_diff"), 
                           labels = c("Inputs (Left Axis)",
                                      "Fates (Left Axis)",
                                      "Water Balance Difference (Right Axis)") ,
                           values = c("green", "red", "black")) +
        scale_linetype_manual(name="",
                              breaks = c("inputs", "fates", "wb_diff"), 
                              labels = c("Inputs (Left Axis)",
                                         "Fates (Left Axis)",
                                         "Water Balance Difference (Right Axis)"),
                              values = c(1, 2, 1)) +
        #scale_x_date(name="Time (YYYY-MM-DD)",date_breaks = "30 days") +
        scale_x_continuous(name = "Time (units)",
                           limits = c(xmin, xmax),
                           breaks = seq(xmin, xmax, xstep)) +
        theme_classic() +
        theme(axis.text.x = element_text(angle = 45, hjust = 1),
              legend.position = "top") +
        ggtitle("Water Balance",
                subtitle=paste0("RMSE = ", 
                                paste0(round(rmse(tdf$inputs,
                                                  tdf$fates),3))))
      return(p)
    },
    .plotWBinv = function(tdf, layer_cols) {
      layer_names <- names(tdf)[layer_cols]
      inv_col <- match(c("time", 
                         "prec",
                         "w_len",
                         "init_w_len",
                         "AE", 
                         "AT_soil_zone", 
                         "deep_perc", 
                         "runoff",
                         layer_names), 
                       names(tdf))
      inv_col <- inv_col[!is.na(inv_col)]
      wbdf <- tdf[, ..inv_col]
      
      prec <- private$.plotPrec(wbdf, "Water Inventory")
      pool <- private$.plotPools(wbdf, layer_names)
      flux <- private$.plotFluxes(wbdf)
      p <- plot_grid(prec, 
                     pool, 
                     flux, 
                     ncol = 1, 
                     align = "v", 
                     rel_heights = c(0.5, 1, 1))
      return(p)
    },
    .plotWBpart = function(tdf) {
      browser()
      inv_col <- match(c("time", 
                         "prec"), 
                       names(tdf))
      inv_col <- inv_col[!is.na(inv_col)]
      wbdf <- tdf[, ..inv_col]
      prec <- private$.plotPrec(wbdf, "Water Partition")
      
      inv_col <- match(c("time",
                         "inputs",
                         "w_len",
                         "AE", 
                         "AT_soil_zone", 
                         "deep_perc", 
                         "runoff"), 
                       names(tdf))
      inv_col <- inv_col[!is.na(inv_col)]
      wbdf <- tdf[, ..inv_col]
      part <- private$.plotPartition(wbdf)
      
      p <- plot_grid(prec, 
                     part, 
                     ncol = 1, 
                     align = "v",
                     rel_heights = c(0.5, 2))
      return(p)
    },
    .plotPrec = function(wbdf, fig_title) {
      # precip plot
      inv_col <- match(c("time", 
                         "prec"), 
                       names(wbdf))
      inv_col <- inv_col[!is.na(inv_col)]
      precip <- wbdf[, ..inv_col]
      precip <- gather(precip, "PoolOrFlux", "units", -"time")
      
      ymax <- max(precip$units)
      ymin <- min(precip$units)
      if(ymax - ymin == 0){
        ystep <- -ymax
      }else{
        ystep <- -ceiling(ymax) / 10
      }
      
      prec <- ggplot(data = precip, aes(x = time)) +
        geom_linerange(aes(ymin = 0, ymax = units), 
                       color = "blue",
                       na.rm = TRUE) +
        scale_y_reverse(limits = c(ceiling(ymax), 0),
                        labels = seq(ceiling(ymax), 0, ystep),
                        breaks = seq(ceiling(ymax), 0, ystep)) +
        theme_classic() +
        theme(axis.title.x = element_blank(),
              axis.text.x = element_blank(),
              axis.ticks.x = element_blank()) +
        labs(y="Precipitation (units)") +
        ggtitle(fig_title)
      return(prec)
    },
    .plotPools = function(wbdf, layer_names) {
      browser()
      # pool plot
      inv_col <- match(c("time",
                         layer_names), 
                       names(wbdf))
      inv_col <- inv_col[!is.na(inv_col)]
      pools <- wbdf[, ..inv_col]
      pools <- gather(pools, "PoolOrFlux", "units", -"time")
      
      xmax <- max(pools$time) #+ 0.25 # for plotting xmax w/ inverse y axis
      xmin <- min(pools$time) #- 0.25 # see ^
      xstep <- ifelse(max(pools$time) > 1,
                      ceiling(max(pools$time) / 10),
                      max(pools$time) / 10)
      ymax <- ceiling_dec(max(pools$units),
                          nchar(round(1 / max(pools$units))))
      ymin <- floor_dec(min(pools$units),
                        nchar(round(1 / min(pools$units))))
      ystep <- (ceiling_dec(max(pools$units),
                            nchar(round(1 / max(pools$units)))) - 
                  floor_dec(min(pools$units),
                            nchar(round(1 / min(pools$units))))
      ) / 10
      
      pools <- private$.legitNames(pools)
      pools$PoolOrFlux <- factor(pools$PoolOrFlux)
      pool_names <- levels(pools$PoolOrFlux)
      
      pool <- ggplot(pools) +
        geom_line(aes(x = time, y = units, color = PoolOrFlux)) +
        scale_y_continuous(name = "Soil Water (length)",
                           limits = c(ymin, ymax),
                           breaks = seq(ymin, ymax, ystep),
                           labels = seq(ymin, ymax, ystep)) +
        #scale_x_date(name="Time (YYYY-MM-DD)",date_breaks = "30 days") +
        scale_x_continuous(name = "Time (units)",
                           limits = c(xmin, xmax),
                           breaks = seq(xmin, xmax, xstep)) + 
        scale_color_discrete(name = "", labels = levels(pools$PoolOrFlux)) +
        theme_classic() +
        theme(legend.position = "top") +
        theme(axis.title.x = element_blank(),
              axis.text.x = element_blank(),
              axis.ticks.x = element_blank())
      return(pool)
    },
    .plotFluxes = function(wbdf) {
      # flux plot
      inv_col <- match(c("time", 
                         "AE", 
                         "AT_soil_zone", 
                         "deep_perc", 
                         "runoff"), 
                       names(wbdf))
      inv_col <- inv_col[!is.na(inv_col)]
      fluxes <- wbdf[, ..inv_col]
      fluxes <- gather(fluxes, "PoolOrFlux", "units", -"time")
      
      xmax <- max(fluxes$time) #+ 0.25 # for plotting xmax w/ inverse y axis
      xmin <- min(fluxes$time) #- 0.25 # see ^
      xstep <- ifelse(max(fluxes$time) > 1,
                      ceiling(max(fluxes$time) / 10),
                      max(fluxes$time) / 10)
      ymax <- ceiling_dec(max(fluxes$units),
                          nchar(round(1 / max(fluxes$units))))
      ymin <- floor_dec(min(fluxes$units),
                        nchar(round(1 / min(fluxes$units))))
      ystep <- (ceiling_dec(max(fluxes$units),
                            nchar(round(1 / max(fluxes$units)))) - 
                  floor_dec(min(fluxes$units),
                            nchar(round(1 / min(fluxes$units))))
      ) / 10
      
      fluxes <- private$.legitNames(fluxes)
      fluxes$PoolOrFlux <- factor(fluxes$PoolOrFlux)
      flux_names <- levels(fluxes$PoolOrFlux)
      
      flux <- ggplot(fluxes, aes(x = time)) +
        geom_line(aes(y = units, color = PoolOrFlux)) + 
        scale_y_continuous(name = "Fate (units)",
                           limits = c(ymin, ymax),
                           breaks = seq(ymin, ymax, ystep),
                           labels = seq(ymin, ymax, ystep)) +
        #scale_x_date(name="Time (YYYY-MM-DD)",date_breaks = "30 days") +
        scale_x_continuous(name = "Time (units)",
                           limits = c(xmin, xmax),
                           breaks = seq(xmin, xmax, xstep)) + 
        scale_color_discrete(name = "",
                             labels = flux_names) +
        theme_classic() +
        theme(legend.position = "top",
              plot.title = element_text(hjust = 0.5)) +
        theme(axis.text.x = element_text(angle = 45, hjust = 1)) 
      return(flux)
    },
    .plotPartition = function(wbdf) {
      fates <- gather(wbdf, "PoolOrFlux", "units", -"time")
      fates <- private$.legitNames(fates)
      fates$PoolOrFlux <- factor(fates$PoolOrFlux)
      fate_names <- levels(fates$PoolOrFlux)
      
      ymax <- ceiling_dec(max(fates$units),nchar(round(1/max(fates$units))))
      ymin <- 0 #floor_dec(min(fates$units),nchar(round(1/min(fates$units))))
      ystep <- (ymax - ymin) / 10
      xmax <- max(fates$time) #+ 0.25 # for plotting xmax w/ inverse y axis
      xmin <- min(fates$time) #- 0.25 # see ^
      xstep <- ifelse(max(fates$time) > 1,
                      ceiling(max(fates$time) / 10),
                      max(fates$time) / 10)
      
      fate <- ggplot(fates, aes(x = time)) +
        geom_line(aes(y = units, color = PoolOrFlux)) +
        scale_y_continuous(name = "Water (length)",
                           limits = c(ymin, ymax),
                           breaks=seq(ymin, ymax, ystep),
                           labels=seq(ymin, ymax, ystep)) + 
        #scale_x_date(name="Time (YYYY-MM-DD)",date_breaks = "30 days") +
        scale_x_continuous(name = "Time (units)",
                           limits = c(xmin, xmax),
                           breaks = seq(xmin, xmax, xstep)) +
        scale_color_discrete(name = "",
                             labels = fate_names) +
        theme_classic() +
        theme(legend.position = "top",
              legend.justification = "center",
              plot.title = element_text(hjust = 0.5)) +
        theme(axis.text.x = element_text(angle = 45, hjust = 1)) 
      return(fate)
    },
    .legitNames = function(df) {
      og_names <- names(df)
      
      if (any(grepl("PoolOrFlux", og_names))) {   
        df <- pivot_wider(df, 
                          names_from = "PoolOrFlux", 
                          values_from = "units")
      } 
      
      if (any(grepl("inputs", names(df)))) {
        names(df)[grep("inputs", names(df))] <- "Prec. + Init. Soil Water"
      }
      if (any(grepl("AE", names(df)))) {
        names(df)[grep("AE", names(df))] <- "Evaporation"
      }
      if (any(grepl("AT_soil_zone", names(df)))) {
        names(df)[grep("AT_soil_zone", names(df))] <- "Transpiration"
      }
      if (any(grepl("deep_perc", names(df)))) {
        names(df)[grep("deep_perc", names(df))] <- "Deep Percolation"
      }
      if (any(grepl("layer_", names(df)))) {
        names(df)[grep("layer_", names(df))] <- paste0("Layer ", 
                                                       str_sub(names(df)[grep("layer_", names(df))], 
                                                               7, 
                                                               nchar(names(df)[grep("layer_", names(df))])),
                                                       " Soil Water")
      }
      if (any(grepl("runoff", names(df)))) {
        names(df)[grep("runoff", names(df))] <- "Surface Runoff"
      }
      if (any(grepl("outputs", names(df)))) {
        names(df)[grep("outputs", names(df))] <- "Outputs"
      }
      if (any(grepl("pec", names(df)))) {
        names(df)[grep("prec", names(df))] <- "Precipitation"
      }
      
      if (any(grepl("PoolOrFlux", og_names))) {   
        df <- gather(df,
                     key="PoolOrFlux",
                     value="units", 
                     -("time"))
      } 
      return(df)
    }
  )
)







