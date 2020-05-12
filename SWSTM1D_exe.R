## SWSTM1D - Soil Water and Solute Transport Model 1D
## Paul Hegedus

## Description:
## Script for loading and implementing the 1D soil transport simulation mod. 
## This is the main .exe script in the 1D soil transport sim model. will not 
## run without proper components in working directory.
## see flowcharts for overview  ... etc. (modSchema vX)
##
##@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@
#### Set up workspace ####
##@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@
## set working directory to script location (Rstudio version)
setwd(
  dirname(
    rstudioapi::getActiveDocumentContext()$path
  )
) 
## source the 'sourceCode' script that checks paths to modules & other locations.
## imports all the appropriate classes, functions, and packages. 
source(
  paste0(
    getwd(),
    "/SWSTM1D_sourceCode.R"
    )
  )
##@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@
#### Set up model  ####
##@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@
# see SWSTM1D_inputGen.R for making req inputs in correct format
# code below runs the script (will overwrite csv's)
#setwd(paste0(getwd(),"/inputs")) # set to inputs folder 
#source(
#  paste0(
#    getwd(),
#    "/SWSTM1D_inputGen.R" # makes the csv's imported below (can manually edit too)
#    )
#  ) 
#setwd(dirname(rstudioapi::getActiveDocumentContext()$path)) # set back to exe location

## ?? initialize outputter ??
#swstm1d_op <- 
#  SWSTM1d_OP$new(
# initialize the outputter 
# TODO: makes the 'outputs' folder
#  )

## initialize the soil sim model
# 1) get data from the 'inputs' folder
# 2) initialize soilModData class
swstm1d <- SWSTM1D$new(
  # time level inputs
  tIn = fread(
    paste0(
      getwd(),
      "/inputs/tIn_dat.csv"
    )
  ),
  # depth level inputs
  zIn = fread(
    paste0(
      getwd(),
      "/inputs/zIn_dat.csv"
    )
  ),
  # table with module names
  mIn = fread(
    paste0(
      getwd(),
      "/inputs/moduleSlctIn_dat.csv"
    )
  )
)

## run the soil sim set up function
swstm1d$setup() ## ?? TODO: loadModules

##@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@
#### Execute simulation ####
##@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@
swstm1d$execute() 

##@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@
#### Plot/Save outputs ####
##@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@
swstm1d$output() 







##@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@
# old visuals (in folders from output() call now)
# swstm1d$output() = swstm1d$outputter$plotFinal
### Visualize Outputs
soilMod1d$soilModData$plot_dpXt() # deep perc vs time
soilMod1d$soilModData$plot_pXt() # precip vs time 

#soilMod1d$soilModData$t_dat # time level data outputs 
#soilMod1d$soilModData$z_dat # depth level data outputs

for(i in 0:sim_length){
  print(soilMod1d$soilModData$plot_vwcXz(i)) # vwc vs depth over time
  i=Sys.sleep(2)
}




