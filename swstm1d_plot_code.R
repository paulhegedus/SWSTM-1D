## Code for Plots
##
## Code for plotting variables that has not been implemented yet.
## Previously in outputters, will be implemented later

## 

## from SWSTM1D_OP
# public - 
# Add plots for z data at each timestep that don't depend on module here
#Zplot_t = function(t) {
#  private$.Plot_VWCxZ(t)
#  
#},
# - end 

# private - 
# Plot vwc by depth for specified timestep 
## TODO: add in depth units
# .Plot_VWCxZ = function(t) {
#   owd <- paste0(self$soilModData$ioPath, "/outputs/zOut/VWCxT") 
#   if (!file.exists(owd)) { 
#     dir.create(owd)
#   }
#   stopifnot(
#     any(grepl("vwc", names(self$soilModData$zDat)))
#   )
#   pd  <- do.call(rbind.data.frame,
#                  lapply(self$soilModData$soilProfile$soilLayers,
#                         as.data.frame))
#   for (i in 1:nrow(pd)) {
#     pd$z_labels[i] <- 
#       ifelse(
#         i == 1,
#         paste0("0 - ", pd$z[i]),
#         paste0(pd$z[i-1], " - ", pd$z[i])
#       )
#   }
#   pd$z_labels <- as.factor(pd$z_labels) %>%
#     fct_rev()
#   p <- ggplot(pd, aes(x = z_labels, y = vwc)) +
#     geom_bar(stat = "identity",
#              color = "darkblue",
#              fill = "darkblue") +
#     scale_y_continuous(limits = c(0, 1), 
#                        breaks = seq(0, 1, 0.1)) +
#     labs(y = "Volumetric Water Content", x = "Depth") +
#     coord_flip() + 
#     theme_classic() +
#     ggtitle(paste0("Time: ", t))
#   ggsave(filename = paste0(self$soilModData$ioPath, 
#                            "/outputs/zOut/VWCxT/VWCxZ_t", 
#                            t, 
#                            ".png"),
#          plot = p,
#          device = "png",
#          scale = 1,
#          width = 5, 
#          height = 7.5, 
#          units = "in")
#   #print(p)
# },

# plot precipitation by time 
## TODO: add in depth & time units
# .Plot_PxT = function() {
#   stopifnot(
#     any(grepl("prec", names(self$soilModData$tDat)))
#   )
#   pd <- self$soilModData$tDat
#   ymax <- RoundTo(max(pd$prec), 1, ceiling)
#   ystep <- - (ymax - 0) / 10
#   xstep <- (max(pd$time) - 0) / 10
#   
#   p <- ggplot(pd, aes(x = time, y = prec)) +
#     geom_bar(stat = "identity",
#              color = "white",
#              fill = "blue") +
#     scale_y_reverse(limits = c(ymax, 0),
#                     labels = seq(ymax, 0, ystep),
#                     breaks = seq(ymax, 0, ystep)) +
#     scale_x_continuous(position = "top",
#                        limits = c(0.5, max(pd$time) + 0.5),
#                        breaks = seq(1, max(pd$time), xstep)) +
#     labs(y = "Precipitation", x = "Time Step") +
#     theme_classic() 
#   
#   ggsave(filename = paste0(self$soilModData$ioPath, 
#                            "/outputs/tOut/PxT_T.png"),
#          plot = p,
#          device = "png",
#          scale = 1,
#          width = 5, 
#          height = 7.5, 
#          units = "in")
#   #print(p)
# }
# 
# - end


## from SWSTM1D_OP
# public - 
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
# - end