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
##------------------
## USER INPUTS
##------------------
# path to model files location
modPath <- "/Users/PaulBriggs/Box/Hegedus/Dissertation/Chapter3/swstm1D/dev/Code"
# path to location of 'inputs' folder; outputs put here (does not need to be same as model)
ioPath <- "/Users/PaulBriggs/Box/Hegedus/Dissertation/Chapter3/swstm1D/dev/Test"
# string of module names
mIn <- c("DrainModuleFC") 
##------------------
## source the 'sourceCode' script with non-module specific R6ClassGenerators &
## package loading & function for checking for model requirements
source(
  paste0(
    modPath,
    "/SWSTM1D_sourceCode.R"
  )
)
## check module & io (inputs/outputs) path
checkForModelReqs(
  modPath,
  ioPath
)
##@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@
#### Set up model  ####
##@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@
## 1) generate SWSTM1D class object
swstm1d <- SWSTM1D$new(
  # path and module info
  modPath = modPath,
  ioPath = ioPath,
  mIn = mIn
)
## 2) run the soil sim set up function
swstm1d$setup() 
##@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@
#### Execute simulation ####
##@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@
## 3) execute the soil sim modules
swstm1d$execute() 
##@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@
#### Plot/Save outputs ####
##@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@
## 4) plot and save final outputs
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




