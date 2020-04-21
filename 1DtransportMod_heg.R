## 1D Water and Solute Transport Model
## Paul Hegedus
##
## Description:
## see flowcharts for overview  ... etc.
##
## v1: drainage module
##
##@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@
## set working directory to script location
setwd(
  dirname(
    rstudioapi::getActiveDocumentContext()$path
  )
) 
## packages and source code
source(paste0(getwd(),"/1DtransportMod_sourceCode_heg_v1.R"))

##@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@
### Inputs ###
## time level inputs
# these will include the timesteps across the simulation period
# with relevant parms. the only required parameter in this table
# is time (the bare min. scenario isjust drainage of water in the 
# soil, nothing else)
sim_length <- 3
t_in <- data.frame(
  time = 1:sim_length#,
  #prec = rep(1,sim_length)
)
## depth level inputs
# provide the inputs specific to each layer for the initial conditions
# t=0. the min. requirements are depth of layer (depth), field capacity (fc),
# and volumetric water content (vwc). Additional parameters can be included 
# for different modules
num_layers <- 5
z_in <- data.frame(
  time = rep(0,num_layers),
  depth = rep(1,num_layers),
  fc = rep(0.1,num_layers),
  vwc = rep(0.5,num_layers)
)
##@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@
## Set Up Model 
# TODO: make it's own class
soilModData <- SoilModData$new(t_in,
                               z_in)
soilProfile <- SoilProfile$new(soilModData$z_dat)
soilProfile$soil_layers

# Run Soil Model
for(t in 1:nrow(soilModData$t_dat)){
  # run drainage module
  DrainModuleFC$
    new()$
    setup(soilModData,soilProfile)$
    calculate(soilProfile)$
    update(soilModData,soilProfile,t)
}

##@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@
## Visualize Outputs
soilModData$plotDPxTime()
soilModData$plotPxTime()

#for(i in 0:sim_length){
#  print(soilModData$plotVWCxDepth(i))
#  Sys.sleep(2)
#}

soilModData$t_dat
#soilModData$z_dat







