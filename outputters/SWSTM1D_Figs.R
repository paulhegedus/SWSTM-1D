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
      browser()
      
      df <- fread(self$t_con, header = TRUE)
      df <- df[-1, ]

      ## MB I/O - Line
      for(i in 1:nrow(tdf)){ 
        if(i==1){
          tdf$inputs[i] <- Pdf$Prec[i] + (sum(subset(ddf,ddf$Day==0)$Moisture)/((depth*disc)+1)*depth)
          tdf$fates[i] <- tdf$Volume[i] + ((sum(subset(ddf,ddf$Day==i)$Moisture)/((depth*disc)+1)*depth)-tdf$Volume[i]) + Pdf$rSoil[i] + tdf$vRoot[i] - tdf$vBot[i] + tdf$RunOff[i]
          tdf$MBdiff[i] <- tdf$inputs[i] - tdf$fates[i]
        }else{
          if(tdf$rTop[i-1]<0){
            tdf$inputs[i] <- Pdf$Prec[i] + tdf$Volume[i-1] # (sum(subset(ddf,ddf$Day==i)$Moisture)/((depth*disc)+1)*100) field capacity at t=0  
            tdf$fates[i] <- tdf$Volume[i] + Pdf$rSoil[i] + tdf$vRoot[i] - tdf$vBot[i] + tdf$RunOff[i]
            tdf$MBdiff[i] <- tdf$inputs[i]-tdf$fates[i]
          }else{
            tdf$inputs[i] <- Pdf$Prec[i] + tdf$Volume[i-1] # 
            tdf$fates[i] <- (sum(subset(ddf,ddf$Day==i)$Moisture)/((depth*disc)+1)*depth) + ((sum(subset(ddf,ddf$Day==i)$Moisture)/((depth*disc)+1)*depth)-tdf$Volume[i]) + Pdf$rSoil[i] + tdf$vRoot[i] - tdf$vBot[i] + tdf$RunOff[i] #tdf$vTop[i]
            tdf$MBdiff[i] <- tdf$inputs[i]-tdf$fates[i]
          }
        }
      }
      wbPlot <- gather(tdf,key="Process",value="cm",-(1:26))
      scl <-  floor_dec(min(subset(wbPlot,wbPlot$Process!="MBdiff")$cm),nchar(round(1/min(subset(wbPlot,wbPlot$Process!="MBdiff")$cm)))
      ) +  (ceiling_dec(max(subset(wbPlot,wbPlot$Process!="MBdiff")$cm),nchar(round(1/max(subset(wbPlot,wbPlot$Process!="MBdiff")$cm)))
      )-floor_dec(min(subset(wbPlot,wbPlot$Process!="MBdiff")$cm),nchar(round(1/min(subset(wbPlot,wbPlot$Process!="MBdiff")$cm)))
      ))/2 # number to center the difference on the scale
      wbPlot[which(wbPlot$Process=="MBdiff"),"cm"] <- wbPlot[which(wbPlot$Process=="MBdiff"),"cm"] + scl 
      wbPlot$Process <- factor(wbPlot$Process)
      wbPlot$Process = factor(wbPlot$Process,levels(wbPlot$Process)[c(2,1,3)])
      
      png(paste0(getwd(),"/Outputs/",vec[1],"/",vec[2],"/",paste0("fig1_",vec[1],"_",vec[2],"_","WB.png")), width=10, height=10, units='in', res=100)
      WBplot <- ggplot(data=wbPlot,aes(x=date,color=Process)) +
        geom_line(aes(y=cm)) +
        scale_y_continuous(name="Water (cm)", limits=c(floor_dec(min(wbPlot$cm),nchar(round(1/min(wbPlot$cm)))),ceiling_dec(max(wbPlot$cm),nchar(round(1/max(wbPlot$cm))))),
                           sec.axis = sec_axis(~.-scl, name = "Water Balance Difference (cm)")) +
        scale_color_manual(name="",breaks=c("inputs","fates","MBdiff"), labels=c("Inputs (Left Axis)","Fates (Left Axis)","Water Balance Difference (Right Axis)") ,values=c("green","red","black")) + 
        scale_x_date(name="Time (YYYY-MM-DD)",date_breaks = "30 days") +
        theme_classic() +
        theme(axis.text.x = element_text(angle = 45, hjust = 1),
              legend.position = "top") +
        ggtitle("Water Balance",subtitle=paste0("RMSE = ",paste0(rmse(tdf$inputs,tdf$fates))))
      print(WBplot)
      dev.off()
      
      
      ## MB Partition - Line x Color
      
      
      
    },
    closeCon = function() {}
  )
  #private = list()
)







